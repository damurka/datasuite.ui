const path = require("path");

// React, ReactDOM and shiny.react's own runtime are provided by shiny.react at page load (window.jsmodule),
// so they are external: this bundle holds only the Countdown components (from @quire/components, in
// ../../datasuite-ui-kit/packages/components, built first by `npm run build`'s prebuild) and the Shiny glue
// here. It is built into the shared UI (apps/_shared/www/cd-react) that every app loads, so running an app
// needs no Node.
const OUTPUTS = [
  { name: "shared", dir: path.resolve(__dirname, "..", "inst", "www", "cd-react") },
];

module.exports = OUTPUTS.map(({ name, dir }) => ({
  name,
  entry: "./src/index.ts",
  output: {
    path: dir,
    filename: "cd-react.js",
  },
  module: {
    rules: [
      {
        test: /\.(t|j)sx?$/,
        exclude: /node_modules/,
        // .babelrc by name: @quire/components (a linked package, outside this folder) is compiled with it too
        use: { loader: "babel-loader", options: { babelrc: false, extends: path.join(__dirname, ".babelrc") } },
      },
    ],
  },
  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"],
  },
  externals: {
    react: "jsmodule['react']",
    "react-dom": "jsmodule['react-dom']",
    "@/shiny.react": "jsmodule['@/shiny.react']",
  },
  devtool: "source-map",
}));
