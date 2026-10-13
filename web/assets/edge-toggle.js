export function attachEdgeToggle(
    host,
    toggle,
    {
        overlap = 12,
        viewportOffset = 12,
        inset = 18,
        size = 36,
        editIcon = "",
        previewIcon = "</>",
        editLabel = "Edit Markdown",
        previewLabel = "Preview rendered page"
    } = {}
) {
    toggle.classList.add("edge-toggle");

    toggle.style.width = `${size}px`;
    toggle.style.height = `${size}px`;

    function setMode(editing) {
        toggle.innerHTML = editing
            ? previewIcon
            : editIcon;

        toggle.setAttribute(
            "aria-label",
            editing ? previewLabel : editLabel
        );

        toggle.title =
            editing ? previewLabel : editLabel;
    }

    function update() {
        if (!host.isConnected || !toggle.isConnected) {
            return;
        }

        const rect = host.getBoundingClientRect();
        const height = toggle.getBoundingClientRect().height;

        if (!height) {
            return;
        }

        const minTop = rect.top + inset;
        const maxTop = rect.bottom - inset - height;

        const top = maxTop < minTop
            ? minTop
            : Math.max(
                minTop,
                Math.min(viewportOffset, maxTop)
            );

        toggle.style.left = `${rect.right - overlap}px`;
        toggle.style.transform = "translateX(0)";

        toggle.style.top = `${top}px`;

        // Hide the toggle when its host is entirely off-screen.
        toggle.style.visibility =
            rect.bottom <= 0 ||
            rect.top >= window.innerHeight
                ? "hidden"
                : "visible";
    }

    toggle.style.position = "fixed";
    toggle.style.zIndex = "1000";

    window.addEventListener("scroll", update, true);
    window.addEventListener("resize", update);

    const observer = new ResizeObserver(update);
    observer.observe(host);
    observer.observe(toggle);

    update();

    return {
        setMode,

        update,

        detach() {
            window.removeEventListener(
                "scroll",
                update,
                true
            );

            window.removeEventListener("resize", update);

            observer.disconnect();
        }
    };
}