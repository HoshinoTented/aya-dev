module aya.lsp {
  requires aya.base;
  requires aya.cli;

  requires static org.jetbrains.annotations;
  requires com.google.gson;
  requires org.eclipse.lsp4j;
  requires org.eclipse.lsp4j.jsonrpc;
  requires info.picocli;

  exports org.aya.lsp.models;
  exports org.aya.lsp.server;
  exports org.aya.lsp.utils;
  exports org.aya.lsp;

  opens org.aya.lsp.models to com.google.gson;
  exports org.aya.lsp.actions;
}
