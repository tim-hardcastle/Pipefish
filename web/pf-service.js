class PipefishService extends HTMLElement {
    constructor() {
        super();

        this.ready = this.loadWasm();
        this.source = null;
    }

    async initialize(source) {
        await this.compile(source);
    }

    async compile(source) {
        await this.ready;

        if (source === this.source) {
            return;
        }

        let files = null;

        const lines = source.replace(/\r\n/g, "\n").split("\n");

        if (lines[0].trim().startsWith("./")) {
            files = await this.loadDirectory(lines);
        } else {
            files = [
                {
                    path: this.pipefishGetFilepath(),
                    data: new TextEncoder().encode(source)
                }
            ];
        }
        const result =
            await window.pipefishCompile(files);

        this.source = source;

        return result;
    }

    async compileMain() {
        await this.ready;
        return window.pipefishCompileMain();
    }

    async loadDirectory(lines) {
        const base = lines[0].trim();
        const entries = this.parseDirectory(lines.slice(1));
        const files = [];

        for (const path of entries) {
            const response =
                await fetch(new URL(path, new URL(base, document.baseURI)), {
                    cache: "no-store"
                });;

            if (!response.ok) {
                throw new Error(
                    `Unable to load ${path}: ${response.status} ${response.statusText}`
                );
            }

            const data =
                new Uint8Array(await response.arrayBuffer());

            files.push({
                path,
                data
            });
        }

        return files;
    }

    parseDirectory(lines) {
        const entries = [];
        const stack = [];

        for (const line of lines) {
            if (!line.trim()) {
                continue;
            }

            const match = line.match(/^(\s*)(.*)$/);
            const indent = match[1].replace(/\t/g, "    ").length;
            const name = match[2].trim();

            while (
                stack.length > 0 &&
                indent <= stack[stack.length - 1].indent
            ) {
                stack.pop();
            }

            const path =
                stack.length > 0
                    ? stack[stack.length - 1].path + name
                    : name;

            if (name.endsWith("/")) {
                stack.push({
                    indent,
                    path
                });
            } else {
                entries.push(path);
            }
        }

        return entries;
    }

    async do(line) {
        await this.ready;

        return window.pipefishDo(line);
    }

    async loadWasm() {
        const wasmURL = "assets/pipefish.wasm";
        const execURL = "assets/wasm_exec.js";

        if (!window.Go) {
            await this.loadScript(execURL);
        }

        window.pipefishWasmReady = false;

        this.go = new Go();

        const result =
            await WebAssembly.instantiateStreaming(
                fetch(wasmURL),
                this.go.importObject
            );

        this.go.run(result.instance);

        while (!window.pipefishWasmReady) {
            await new Promise(resolve =>
                setTimeout(resolve, 0)
            );
        }
    }

    loadScript(url) {
        return new Promise((resolve, reject) => {
            const script =
                document.createElement("script");

            script.src = url;
            script.onload = resolve;
            script.onerror = reject;

            document.head.appendChild(script);
        });
    }
}

customElements.define(
    "pf-service",
    PipefishService
);