/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.gleam"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/forms")],
};
