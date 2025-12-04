import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

export default defineConfig({
  plugins: [wasm(), topLevelAwait()],
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
