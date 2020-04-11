import purs from "rollup-plugin-purs";
import { terser } from "rollup-plugin-terser";

export default {
  entry: "src/Main.purs",
  dest: "dist/rollup.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs(),
    terser()
  ]
};