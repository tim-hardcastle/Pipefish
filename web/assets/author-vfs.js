const SAMPLE_BOOK_FILES = [
    "Contents.md",
    "Components-overview.md",
    "Initializing-components.md",
    "Introduction.md",
    "Markdown.md",
    "The-Books-menu.md",
    "The-Files-menu.md",
    "The-structure-of-a-book.md"
];

export class AuthorVFS {
    async initialize() {
        const files = await Promise.all(
            SAMPLE_BOOK_FILES.map(async name => {
                const path = `using-publisher/${name}`;
                const url = new URL(
                    `using-publisher/${name}`,
                    import.meta.url
                );

                const response = await fetch(url);

                if (!response.ok) {
                    throw new Error(
                        `Failed to load ${path}: ${response.status}`
                    );
                }

                return {
                    path,
                    data: new TextEncoder().encode(await response.text())
                };
            })
        );

        await window.pipefishReady;

        const result = window.pipefishInitAuthorFiles(files);

        if (!result?.ok) {
            throw new Error(
                result?.error ?? "Failed to initialize author files."
            );
        }
    }

    getTree() {
        const result = window.pipefishGetAuthorFileTree();

        if (result?.error) {
            throw new Error(result.error);
        }

        return result;
    }

    openBook(path) {
        const result = window.pipefishOpenBook(path);

        if (!result?.ok) {
            throw new Error(
                result?.error ?? `Failed to open book: ${path}`
            );
        }

        return window.pipefishGetBookFileTree();
    }
}