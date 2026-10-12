export function attachEdgeToggle(
    host,
    toggle,
    {
        overlap = 16,
        viewportOffset = 12,
        inset = 17
    } = {}
) {
    function update() {
        if (!host.isConnected || !toggle.isConnected) {
            return;
        }

        const rect = host.getBoundingClientRect();
        const height = toggle.getBoundingClientRect().height;

        if (!height) {
            return;
        }

        // Keep the toggle within the host's vertical bounds,
        // while positioning it as near the viewport top as possible.
        const minTop = rect.top + inset;
        const maxTop = rect.bottom - height - inset;

        const top = maxTop < minTop
            ? minTop
            : Math.max(
                minTop,
                Math.min(viewportOffset, maxTop)
            );

        // Overlap the host's right edge.
        const left = rect.right - overlap;

        toggle.style.position = "fixed";
        toggle.style.left = `${left}px`;
        toggle.style.top = `${top}px`;
    }

    // Capture scrolling from nested scrolling containers as well.
    window.addEventListener("scroll", update, true);
    window.addEventListener("resize", update);

    const observer = new ResizeObserver(update);

    observer.observe(host);
    observer.observe(toggle);

    update();

    // Return a cleanup function for the caller.
    return () => {
        window.removeEventListener("scroll", update, true);
        window.removeEventListener("resize", update);

        observer.disconnect();
    };
}