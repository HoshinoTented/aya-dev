// Copyright (c) 2020-2021 Yinsen (Tesla) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
package org.aya.cli.library;

import kala.collection.immutable.ImmutableSeq;
import kala.collection.mutable.DynamicLinkedSeq;
import kala.collection.mutable.DynamicSeq;
import kala.collection.mutable.MutableSet;
import kala.tuple.Unit;
import org.aya.api.error.Reporter;
import org.aya.api.error.SourceFileLocator;
import org.aya.cli.library.json.LibraryConfig;
import org.aya.cli.library.json.LibraryConfigData;
import org.aya.cli.library.json.LibraryDependency;
import org.aya.cli.single.CliReporter;
import org.aya.concrete.resolve.ResolveInfo;
import org.aya.concrete.resolve.module.CachedModuleLoader;
import org.aya.concrete.resolve.module.FileModuleLoader;
import org.aya.concrete.resolve.module.ModuleListLoader;
import org.aya.concrete.resolve.module.ModuleLoader;
import org.aya.core.def.Def;
import org.aya.core.serde.Serializer;
import org.aya.util.MutableGraph;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Function;
import java.util.stream.IntStream;

/**
 * @author kiva
 */
public record LibraryCompiler(@NotNull Path buildRoot, boolean unicode) {
  public static int compile(@NotNull Path libraryRoot, boolean unicode) throws IOException {
    var config = LibraryConfigData.fromLibraryRoot(libraryRoot);
    new LibraryCompiler(config.libraryBuildRoot(), unicode).make(config);
    return 0;
  }

  private @Nullable LibraryConfig depConfig(@NotNull LibraryDependency dep) throws IOException {
    // TODO: test only: dependency resolving should be done in package manager
    if (dep instanceof LibraryDependency.DepFile file)
      return LibraryConfigData.fromDependencyRoot(file.depRoot(), version -> depBuildRoot(dep.depName(), version));
    return null;
  }

  private @NotNull Path depBuildRoot(@NotNull String depName, @NotNull String version) {
    return buildRoot.resolve("deps").resolve(depName + "_" + version);
  }

  private @NotNull ImmutableSeq<String> moduleName(@NotNull SourceFileLocator locator, @NotNull Path file) {
    var display = locator.displayName(file);
    var displayNoExt = display.resolveSibling(display.getFileName().toString().replaceAll("\\.aya", ""));
    return IntStream.range(0, displayNoExt.getNameCount())
      .mapToObj(i -> displayNoExt.getName(i).toString())
      .collect(ImmutableSeq.factory());
  }

  private @NotNull DynamicLinkedSeq<Path> collectSource(@NotNull Path srcRoot) throws IOException {
    return Files.walk(srcRoot).filter(Files::isRegularFile)
      .filter(path -> path.getFileName().toString().endsWith(".aya"))
      .map(ResolveInfo::canonicalize)
      .collect(DynamicLinkedSeq.factory());
  }

  private void collectDep(@NotNull MutableGraph<ResolveInfo> dep, @NotNull ResolveInfo info) {
    dep.suc(info).appendAll(info.imports());
    info.imports().forEach(i -> collectDep(dep, i));
  }

  private void collectChanged(
    @NotNull MutableGraph<ResolveInfo> usage,
    @NotNull ResolveInfo changed,
    @NotNull MutableSet<ResolveInfo> changedList
  ) {
    if (changedList.contains(changed)) return;
    changedList.add(changed);
    usage.suc(changed).forEach(dep -> collectChanged(usage, dep, changedList));
  }

  private @NotNull ResolveInfo resolveModule(
    @NotNull ModuleLoader moduleLoader,
    @NotNull SourceFileLocator locator,
    @NotNull Path file
  ) {
    var mod = moduleName(locator, file);
    System.out.printf("  [Resolve] %s (%s)%n", mod.joinToString("::"), file);
    var resolveInfo = moduleLoader.load(mod);
    if (resolveInfo == null) throw new IllegalStateException("Unable to load module: " + mod);
    return resolveInfo;
  }

  /** Produces tyck order of modules in a library */
  private @NotNull MutableGraph<ResolveInfo> resolveLibrary(
    @NotNull ModuleLoader moduleLoader,
    @NotNull SourceFileLocator locator,
    @NotNull LibraryConfig config
  ) throws IOException {
    System.out.println("  [Info] Collecting source files");
    var remaining = collectSource(config.librarySrcRoot());
    var graph = MutableGraph.<ResolveInfo>create();
    if (!remaining.isNotEmpty()) return graph;

    while (remaining.isNotEmpty()) {
      var file = remaining.pop();
      var resolveInfo = resolveModule(moduleLoader, locator, file);
      resolveInfo.imports().forEach(r -> {
        var path = r.canonicalPath();
        remaining.removeAll(path::equals);
      });
      collectDep(graph, resolveInfo);
    }
    return graph;
  }

  private void make(@NotNull LibraryConfig config) throws IOException {
    // TODO[kiva]: move to package manager
    var modulePath = DynamicSeq.<Path>create();
    var locatorPath = DynamicSeq.<Path>create();
    for (var dep : config.deps()) {
      var depConfig = depConfig(dep);
      if (depConfig == null) {
        System.out.println("Skipping " + dep.depName());
        continue;
      }
      make(depConfig);
      modulePath.append(depConfig.libraryOutRoot());
      locatorPath.append(depConfig.librarySrcRoot());
    }

    System.out.println("Compiling " + config.name());

    var srcRoot = config.librarySrcRoot();
    var outRoot = config.libraryOutRoot();
    modulePath.prepend(Files.createDirectories(outRoot));
    modulePath.append(srcRoot);
    locatorPath.prepend(srcRoot);
    var locator = new SourceFileLocator.Module(locatorPath.view());

    var reporter = CliReporter.stdio(unicode);
    var loader = new ModuleListLoader(modulePath.view().map(path ->
      new CachedModuleLoader(new LibraryModuleLoader(locator, path, reporter))).toImmutableSeq());
    var depGraph = resolveLibrary(loader, locator, config);
    var timestamp = new Timestamp(locator, outRoot);
    var coreSaver = new CoreSaver(timestamp);
    recompile(reporter, depGraph, coreSaver, timestamp);
  }

  private void recompile(
    @NotNull Reporter reporter,
    @NotNull MutableGraph<ResolveInfo> depGraph,
    @NotNull CoreSaver coreSaver,
    @NotNull Timestamp timestamp
  ) {
    var changed = MutableSet.<ResolveInfo>create();
    var usage = depGraph.transpose();
    depGraph.E().keysView().forEach(s -> {
      if (timestamp.sourceModified(s.canonicalPath()))
        collectChanged(usage, s, changed);
    });

    if (changed.isEmpty()) {
      System.out.println("  [Info] No changes detected, no need to remake");
      return;
    }

    var incrementalDep = MutableGraph.<ResolveInfo>create();
    changed.forEach(c -> collectDep(incrementalDep, c));
    incrementalDep.topologicalOrder().flatMap(Function.identity()).forEach(mod -> {
      System.out.println("  [Tyck] " + mod.thisModule().underlyingFile());
      FileModuleLoader.tyckResolvedModule(mod, reporter,
        (moduleResolve, stmts, defs) -> coreSaver.saveCompiledCore(moduleResolve, defs),
        null);
    });
  }

  private static @NotNull Path coreFile(
    @NotNull SourceFileLocator locator, @NotNull Path file, @NotNull Path outRoot
  ) throws IOException {
    var raw = outRoot.resolve(locator.displayName(file));
    var core = raw.resolveSibling(raw.getFileName().toString() + "c");
    Files.createDirectories(core.getParent());
    return core;
  }

  record Timestamp(@NotNull SourceFileLocator locator, @NotNull Path outRoot) {
    public boolean sourceModified(@NotNull Path file) {
      try {
        var core = coreFile(locator, file, outRoot);
        if (!Files.exists(core)) return true;
        return Files.getLastModifiedTime(file)
          .compareTo(Files.getLastModifiedTime(core)) > 0;
      } catch (IOException ignore) {
        return true;
      }
    }

    public void update(@NotNull Path file) {
      try {
        var core = coreFile(locator, file, outRoot);
        Files.setLastModifiedTime(core, Files.getLastModifiedTime(file));
      } catch (IOException ignore) {
      }
    }
  }

  record CoreSaver(@NotNull Timestamp timestamp) {
    private void saveCompiledCore(@NotNull ResolveInfo resolveInfo, @NotNull ImmutableSeq<Def> defs) {
      var sourcePath = resolveInfo.canonicalPath();
      try (var outputStream = openCompiledCore(sourcePath)) {
        var serDefs = defs.map(def -> def.accept(new Serializer(new Serializer.State()), Unit.unit()));
        var compiled = CompiledAya.from(serDefs);
        outputStream.writeObject(compiled);
        timestamp.update(sourcePath);
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    private @NotNull ObjectOutputStream openCompiledCore(@NotNull Path sourcePath) throws IOException {
      return new ObjectOutputStream(Files.newOutputStream(
        coreFile(timestamp.locator, sourcePath, timestamp.outRoot)));
    }
  }
}
