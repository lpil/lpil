/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.gleam", "priv/static/**/*.js"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/forms")],
};
