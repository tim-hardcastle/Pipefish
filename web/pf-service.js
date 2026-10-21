import { loadWasm } from "./assets/pf-wasm.js";

class PipefishService extends HTMLElement {
    constructor() {
        super();

        this.ready = loadWasm();
        this.source = null;
        this.fileExtractor = async (path, filename) => {
            const url = new URL(
                filename,
                new URL(
                    path,
                    new URL("book/", document.baseURI)
                )
            );

            const response = await fetch(url, {
                cache: "no-store"
            });

            if (!response.ok) {
                throw new Error(
                    `Unable to load ${url}: ` +
                    `${response.status} ${response.statusText}`
                );
            }

            return new Uint8Array(await response.arrayBuffer());
        };
    }

    async initialize(source) {
        this.initialization = (async () => {
            this.source = source;
        })();
    }

    async build(fileExtractor) {
        await this.initialization
        if (fileExtractor) {
            this.fileExtractor = fileExtractor
        }
        await this.compile(this.source)
    }

    async compile(source) {
        await this.ready;

        let files = null;

        const lines = source.replace(/\r\n/g, "\n").split("\n");

        if (lines[0].trim().startsWith("./")) {
            files = await this.loadDirectory(lines);
        } else {
            files = [
                {
                    path: "main.pf",
                    data: new TextEncoder().encode(source)
                }
            ];
        }
        const result =
            await window.pipefishCompile(files);

        this.source = source;
        const fp = await this.getFilepath()
        return result;
    }

        async loadDirectory(lines) {
        const base = lines[0].trim();
        const entries = this.parseDirectory(lines.slice(1));
        const files = [];

        for (const path of entries) {
            const data = await this.fileExtractor(base, path);

            files.push({
                path,
                data
            });
        }

        return files;
    }

    async compileMain() {
        await this.ready;
        return window.pipefishCompileMain();
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

    async getFilepath() {
        return window.pipefishGetFilepath();
    }

}

customElements.define(
    "pf-service",
    PipefishService
);

