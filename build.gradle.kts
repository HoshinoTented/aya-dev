// Copyright (c) 2020-2022 Tesla (Yinsen) Zhang.
// Use of this source code is governed by the MIT license that can be found in the LICENSE.md file.
import org.apache.tools.ant.taskdefs.condition.Os
import org.aya.gradle.BuildUtil
import java.util.*

plugins {
  java
  `jacoco-report-aggregation`
  idea
  `java-library`
  `maven-publish`
  signing
  id("org.beryx.jlink") version "2.25.0" apply false
}

var deps: Properties by rootProject.ext

deps = Properties()
file("gradle/deps.properties").reader().use(deps::load)

allprojects {
  group = "org.aya-prover"
  version = deps.getProperty("version.project")
}

@Suppress("unsupported")
val useJacoco = ["base", "pretty", "cli"]

/** gradle.properties or environmental variables */
fun propOrEnv(name: String): String =
  if (hasProperty(name)) property(name).toString()
  else (System.getenv(name) ?: "")

subprojects {
  val proj = this@subprojects
  val isSnapshot = proj.version.toString().endsWith("SNAPSHOT")
  val isRelease = !isSnapshot

  apply {
    plugin("java")
    plugin("idea")
    if (name in useJacoco) plugin("jacoco")
    plugin("maven-publish")
    plugin("java-library")
    plugin("signing")
  }

  val javaVersion = 19
  java {
    withSourcesJar()
    if (isRelease) withJavadocJar()
    JavaVersion.toVersion(javaVersion).let {
      sourceCompatibility = it
      targetCompatibility = it
    }
    toolchain {
      languageVersion.set(JavaLanguageVersion.of(javaVersion))
      vendor.set(JvmVendorSpec.BELLSOFT)
    }
  }

  if (name in useJacoco) jacoco {
    toolVersion = deps.getProperty("version.jacoco")
  }

  idea.module {
    outputDir = file("out/production")
    testOutputDir = file("out/test")
  }

  tasks.withType<JavaCompile>().configureEach {
    modularity.inferModulePath.set(true)

    options.apply {
      encoding = "UTF-8"
      isDeprecation = true
      release.set(javaVersion)
      compilerArgs.addAll(listOf("-Xlint:unchecked", "--enable-preview"))
    }

    doLast {
      val tree = fileTree(destinationDirectory)
      tree.include("**/*.class")
      // tree.exclude("module-info.class")
      val root = project.buildDir.toPath().resolve("classes/java/main")
      tree.forEach { BuildUtil.stripPreview(root, it.toPath()) }
    }
  }

  tasks.withType<Javadoc>().configureEach {
    val options = options as StandardJavadocDocletOptions
    options.modulePath = tasks.compileJava.get().classpath.toList()
    options.addBooleanOption("-enable-preview", true)
    options.addStringOption("-source", javaVersion.toString())
    options.addStringOption("Xdoclint:none", "-quiet")
    options.encoding("UTF-8")
    options.tags(
      "apiNote:a:API Note:",
      "implSpec:a:Implementation Requirements:",
      "implNote:a:Implementation Note:",
    )
  }

  artifacts {
    add("archives", tasks.named("sourcesJar"))
    if (isRelease) add("archives", tasks.named("javadocJar"))
  }

  if (name in useJacoco) tasks.jacocoTestReport {
    dependsOn(tasks.test)
    reports {
      xml.required.set(true)
      csv.required.set(false)
      html.required.set(false)
    }
  }

  tasks.withType<Test>().configureEach {
    jvmArgs = listOf("--enable-preview")
    useJUnitPlatform()
    enableAssertions = true
    reports.junitXml.mergeReruns.set(true)
  }

  tasks.withType<JavaExec>().configureEach {
    jvmArgs = listOf("--enable-preview")
    enableAssertions = true
  }

  val ossrhUsername = propOrEnv("ossrhUsername")
  val ossrhPassword = propOrEnv("ossrhPassword")

  if (ossrhUsername.isNotEmpty()) publishing.repositories {
    maven {
      val releasesRepoUrl = uri("https://s01.oss.sonatype.org/service/local/staging/deploy/maven2")
      val snapshotsRepoUrl = uri("https://s01.oss.sonatype.org/content/repositories/snapshots/")
      url = if (isRelease) releasesRepoUrl else snapshotsRepoUrl
      name = "MavenCentral"
      credentials {
        username = ossrhUsername
        password = ossrhPassword
      }
    }
  }

  // Gradle module metadata contains Gradle JVM version, disable it
  tasks.withType<GenerateModuleMetadata>().configureEach {
    enabled = false
  }

  publishing.publications {
    create<MavenPublication>("maven") {
      val githubUrl = "https://github.com/aya-prover/aya-dev"
      groupId = proj.group.toString()
      version = proj.version.toString()
      artifactId = proj.name
      from(components["java"])
      pom {
        description.set("The Aya proof assistant")
        name.set(proj.name)
        url.set("https://www.aya-prover.org")
        licenses {
          license {
            name.set("MIT")
            url.set("$githubUrl/blob/master/LICENSE")
          }
        }
        developers {
          fun dev(i: String, n: String, u: String) = developer {
            id.set(i)
            name.set(n)
            email.set(u)
          }
          dev("ice1000", "Tesla (Yinsen) Zhang", "ice1000kotlin@foxmail.com")
          dev("imkiva", "Kiva Oyama", "imkiva@islovely.icu")
          dev("re-xyr", "Xy Ren", "xy.r@outlook.com")
          dev("dark-flames", "Darkflames", "dark_flames@outlook.com")
          dev("tsao-chi", "tsao-chi", "tsao-chi@the-lingo.org")
          dev("lunalunaa", "Luna Xin", "luna.xin@outlook.com")
          dev("wsx", "Shuxian Wang", "wsx@berkeley.edu")
        }
        scm {
          connection.set("scm:git:$githubUrl")
          url.set(githubUrl)
        }
      }
    }
  }

  if (hasProperty("signing.keyId") && isRelease) signing {
    useGpgCmd()
    sign(publishing.publications["maven"])
  }
}

apply { plugin("jacoco-report-aggregation") }
dependencies { useJacoco.forEach { jacocoAggregation(project(":$it")) { isTransitive = false } } }

val ccr = tasks["testCodeCoverageReport"]
tasks.register("githubActions") {
  group = "verification"
  dependsOn(ccr, tasks.findByPath(":lsp:jlink"))
}

if (Os.isFamily(Os.FAMILY_WINDOWS)) tasks.register("showCCR") {
  dependsOn(ccr)
  doLast { exec { commandLine("cmd", "/c", "explorer", "build\\reports\\jacoco\\testCodeCoverageReport\\html\\index.html") } }
}

tasks.withType<Sync>().configureEach {
  dependsOn(tasks.findByPath(":buildSrc:copyModuleInfo"))
}
