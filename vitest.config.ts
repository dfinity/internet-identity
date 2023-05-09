import { configDefaults, defineConfig } from 'vitest/config'
import {resolve} from "path";

export default defineConfig({
    resolve: {
        alias: {
            // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
            stream: "stream-browserify",
            // Custom alias we are using to shorten and make absolute the imports
            $generated: resolve(__dirname, "src/frontend/generated"),
            $src: resolve(__dirname, "src/frontend/src"),
        },
    },
    test: {
        environment: 'jsdom',
        exclude: [...configDefaults.exclude, 'src/frontend/src/test-e2e/**'],
        globals: true,
        watch: false,
        setupFiles: './src/frontend/test-setup.ts'
    }
})