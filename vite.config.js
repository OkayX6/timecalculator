import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  plugins: [react(), tailwindcss()],
  // To make it work when deployed to GitHub Pages
  base: "./",
  root: "./src",
  build: {
    outDir: "../dist",
  }
})
