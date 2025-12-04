import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'

export default defineConfig({
  plugins: [wasm()],
  base: '/crumble/',  // GitHub Pages base path
  build: {
    target: 'esnext',
  },
  optimizeDeps: {
    exclude: ['crumble'],
  },
  server: {
    fs: {
      // Allow serving files from the parent directory (for the WASM package)
      allow: ['..'],
    },
  },
})
