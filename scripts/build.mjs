import esbuild from "esbuild";

esbuild
  .build({
    entryPoints: ["./scripts/entry.js"],
    bundle: true,
    outfile: "public/index.js",
    minify: true,
    sourcemap: false,
    target: ["chrome70", "firefox57", "safari11", "edge18"],
    watch: false,
    logLevel: "info",
  })
  .catch((e) => {
    console.error(e);
    process.exit(1);
  });
