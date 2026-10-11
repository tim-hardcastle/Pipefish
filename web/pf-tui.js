import "./pf-service.js";
import "./pf-highlighter.js";

class PipefishTui extends HTMLElement {
    constructor() {
        super();

        const highlighter =
            document.createElement("pf-highlighter");

        this.service = null;
        this.highlighter = highlighter;
        this.ready = highlighter.ready;
        this.initialized = Promise.resolve();

        this.history = [];
        this.historyIndex = 0;
        this.historyDraft = "";
        this.multiline = false;

        const shadow =
            this.attachShadow({ mode: "open" });

        const style =
            document.createElement("link");
        style.rel = "stylesheet";
        style.href =
            new URL("../assets/pf-tui.css", import.meta.url);

        const repl =
            document.createElement("div");
        repl.classList.add("repl");

        const transcript =
            document.createElement("div");
        transcript.classList.add("transcript");

        this.transcript = transcript;
        this.repl = repl;

        const inputLine =
            document.createElement("div");
        inputLine.classList.add("input-line");

        const prompt = document.createElement("span");
        prompt.classList.add("prompt");

        this.promptLabel = document.createElement("span");
        this.promptLabel.classList.add("cyan");

        const arrow = document.createElement("span");
        arrow.textContent = " → ";

        prompt.append(this.promptLabel, arrow);

        this.prompt = prompt

        const inputEditor =
            document.createElement("div");
        inputEditor.classList.add("input-editor");

        const highlightedInput =
            document.createElement("pre");
        highlightedInput.classList.add("highlighted-input");

        const input =
            document.createElement("textarea");
        input.classList.add("input");
        input.autocomplete = "off";
        input.spellcheck = false;
        input.rows = 1;

        inputEditor.append(
            highlightedInput,
            input
        );

        inputLine.append(
            prompt,
            inputEditor
        );

        this.input = input;
        this.highlightedInput = highlightedInput;
        this.inputEditor = inputEditor;

        input.addEventListener("input", async () => {
            await this.ready;

            highlightedInput.innerHTML =
                await this.highlighter.highlight(
                    input.value
                );

            this.updateInputHeight();
            input.scrollTop = input.scrollHeight;
            highlightedInput.scrollTop =
                input.scrollTop;
            this.scrollToInput();
        });

        input.addEventListener("scroll", () => {
            this.syncEditorScroll(
                input,
                highlightedInput
            );
        });

        input.addEventListener(
            "keydown",
            async event => {
                if (event.key === "Tab") {
                    event.preventDefault();

                    input.setRangeText(
                        "\t",
                        input.selectionStart,
                        input.selectionEnd,
                        "end"
                    );

                    input.dispatchEvent(
                        new Event("input")
                    );

                    return;
                }

                if (event.key === "ArrowUp") {
                    event.preventDefault();

                    if (this.historyIndex === this.history.length) {
                        this.historyDraft = input.value;
                    }

                    if (this.historyIndex > 0) {
                        this.historyIndex--;
                        input.value = this.history[this.historyIndex];
                        input.dispatchEvent(new Event("input"));
                        input.setSelectionRange(
                            input.value.length,
                            input.value.length
                        );
                    }

                    return;
                }

                if (event.key === "ArrowDown") {
                    event.preventDefault();

                    if (this.historyIndex < this.history.length - 1) {
                        this.historyIndex++;
                        input.value = this.history[this.historyIndex];
                    } else {
                        this.historyIndex = this.history.length;
                        input.value = this.historyDraft;
                    }

                    input.dispatchEvent(new Event("input"));
                    input.setSelectionRange(
                        input.value.length,
                        input.value.length
                    );

                    return;
                }

                if (this.handleDelimiter(event, input)) {
                    return;
                }

                if (event.key !== "Enter") {
                    return;
                }

                const start =
                    input.selectionStart;

                const before =
                    input.value.slice(0, start);

                const currentLine =
                    before.split("\n").pop();

                if (!this.multiline) {
                    if (
                        /(:\s*|--\s*)$/.test(
                            currentLine
                        )
                    ) {
                        event.preventDefault();

                        input.setRangeText(
                            "\n" +
                            this.replIndent(currentLine),
                            input.selectionStart,
                            input.selectionEnd,
                            "end"
                        );

                        this.multiline = true;

                        input.dispatchEvent(
                            new Event("input")
                        );

                        return;
                    }

                    event.preventDefault();

                    await this.submitReplInput();
                    return;
                }

                if (!currentLine.trim()) {
                    event.preventDefault();

                    await this.submitReplInput();
                    return;
                }

                event.preventDefault();

                input.setRangeText(
                    "\n" +
                    this.replIndent(currentLine),
                    input.selectionStart,
                    input.selectionEnd,
                    "end"
                );

                input.dispatchEvent(
                    new Event("input")
                );
            }
        );

        repl.append(
            transcript,
            inputLine
        );

        shadow.append(
            style,
            repl
        );
    }

    updateInputHeight() {
        const lineHeight = 14 * 1.4;
        const maxHeight = lineHeight * 12;

        this.inputEditor.style.height = "auto";

        const height =
            Math.min(this.input.scrollHeight, maxHeight);

        this.inputEditor.style.height =
            `${height}px`;
    }

    scrollToInput() {
        const bottom =
            this.inputEditor.offsetTop +
            this.inputEditor.offsetHeight;

        const visibleBottom =
            this.repl.scrollTop +
            this.repl.clientHeight;

        if (bottom > visibleBottom) {
            this.repl.scrollTop =
                bottom - this.repl.clientHeight;
        }
    }


    get service() {
        if (this._service === null) {
            this._service =
                document.createElement("pf-service");
        }

        return this._service;
    }

    set service(service) {
        this._service = service;
    }

    async initialize(source) {
        this.initialized = this._initialize(source);
        return this.initialized;
    }

    async _initialize(source) {
        await this.ready;
        await this.service.initialize(source);
        const fp = await this.service.getFilepath()
        this.promptLabel.textContent = getFilenameWithoutExtension(fp)
    }

    isEscaped(text, pos) {
        let backslashes = 0;

        for (
            let i = pos - 1;
            i >= 0 && text[i] === "\\";
            i--
        ) {
            backslashes++;
        }

        return backslashes % 2 === 1;
    }

    quoteContext(text, pos, quote) {
        let currentQuote = null;
        let escaped = false;

        for (let i = 0; i < pos; i++) {
            const char = text[i];

            if (escaped) {
                escaped = false;
                continue;
            }

            if (char === "\\") {
                escaped = true;
                continue;
            }

            if (currentQuote === null) {
                if (
                    char === '"' ||
                    char === "'" ||
                    char === "`"
                ) {
                    currentQuote = char;
                }
            } else if (char === currentQuote) {
                currentQuote = null;
            }
        }

        return currentQuote === quote;
    }

    handleDelimiter(event, element) {
        const pairs = {
            "(": ")",
            "[": "]",
            "{": "}",
            '"': '"',
            "'": "'",
            "`": "`"
        };

        const closingDelimiters = new Set([
            ")",
            "]",
            "}",
            '"',
            "'",
            "`"
        ]);

        const isQuote =
            event.key === '"' ||
            event.key === "'" ||
            event.key === "`";

        if (closingDelimiters.has(event.key)) {
            const pos = element.selectionStart;

            if (
                pos === element.selectionEnd &&
                element.value[pos] === event.key
            ) {
                if (!isQuote) {
                    event.preventDefault();

                    element.setSelectionRange(
                        pos + 1,
                        pos + 1
                    );

                    return true;
                }

                if (
                    this.quoteContext(
                        element.value,
                        pos,
                        event.key
                    ) &&
                    !this.isEscaped(
                        element.value,
                        pos
                    )
                ) {
                    event.preventDefault();

                    element.setSelectionRange(
                        pos + 1,
                        pos + 1
                    );

                    return true;
                }
            }
        }

        if (pairs[event.key] !== undefined) {
            const start = element.selectionStart;
            const end = element.selectionEnd;

            if (
                isQuote &&
                this.quoteContext(
                    element.value,
                    start,
                    event.key
                )
            ) {
                return false;
            }

            event.preventDefault();

            const selected =
                element.value.slice(start, end);

            element.setRangeText(
                event.key +
                selected +
                pairs[event.key],
                start,
                end,
                "select"
            );

            element.setSelectionRange(
                start + 1,
                start + 1 + selected.length
            );

            element.dispatchEvent(
                new Event("input")
            );

            return true;
        }

        return false;
    }

    replIndent(line) {
        const indent =
            line.match(/^[\t ]*/)[0];

        if (/(:\s*|--\s*)$/.test(line)) {
            return indent + "\t";
        }

        return indent;
    }

    async submitReplInput() {
        let command = this.input.value;

        if (this.multiline) {
            const lines = command.split("\n");

            if (!lines[lines.length - 1].trim()) {
                lines.pop();
            }

            command = lines.join("\n");
        }

        this.multiline = false;

        this.input.value = "";
        this.highlightedInput.innerHTML = "";

        await this.executeReplCommand(command);
    }

    async executeReplCommand(command) {
        await this.initialized;
        this.history.push(command);
        this.historyIndex = this.history.length;
        this.historyDraft = "";
        const entry =
            document.createElement("pre");

        entry.classList.add("transcript-input");

        entry.append(
            this.prompt.cloneNode(true),
            Object.assign(document.createElement("span"), {
                innerHTML: await this.highlighter.highlight(command)
            })
        );

        this.transcript.appendChild(entry);

        try {
            await this.service.ready;

            const result =
                await this.service.do(command);

            if (
                result !== undefined &&
                result !== ""
            ) {
                this.write(result);
            }
        } catch (error) {
            this.write(
                "Error: " + error
            );
        }

        this.scrollToBottom();
    }

    write(text) {
        const line =
            document.createElement("div");

        line.innerHTML = text;

        this.transcript.appendChild(line);
    }

    scrollToBottom() {
        this.repl.scrollTop =
            this.repl.scrollHeight;
    }

    syncEditorScroll(input, highlighted) {
        highlighted.scrollTop =
            input.scrollTop;

        highlighted.scrollLeft =
            input.scrollLeft;
    }
}

function getFilenameWithoutExtension(pathString) {
    const baseName = pathString.split(/[\\/]/).pop();
    const lastDotIndex = baseName.lastIndexOf('.');
    if (lastDotIndex < 1) return baseName;
    return baseName.slice(0, lastDotIndex);
}

customElements.define(
    "pf-tui",
    PipefishTui
);