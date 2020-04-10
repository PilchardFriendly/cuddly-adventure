import purs from "rollup-plugin-purs";

export default {
  entry: "src/Main.purs",
  dest: "dist/rollup.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs()
  ]
};