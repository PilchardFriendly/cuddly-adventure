import purs from "rollup-plugin-purs";
import { terser } from "rollup-plugin-terser";

export default {
  input: "src/Main.purs",
  output: {
    file: "dist/rollup.min.js"
    ,format: "iife"
    ,sourcemap: true
  },
  plugins: [
    purs({runMain:true})
    ,terser()
  ]
};