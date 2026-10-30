"""Recon of the live Space: screenshots of the main states, with no paid call.

Only the saved runs of the unchanged samples are run (they cost nothing).
Run from video/:  python recon.py
"""
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE = "https://fmegahed-decisions.hf.space/"
OUT = Path(__file__).resolve().parent / "captures" / "recon"
VIEW = {"width": 1600, "height": 720}


def shot(page, name, full=False):
    page.screenshot(path=str(OUT / f"{name}.png"), full_page=full)
    print("shot", name)


def idle(page, timeout=60):
    """Wait until Shiny is not busy."""
    deadline = time.time() + timeout
    time.sleep(0.6)
    while time.time() < deadline:
        if not page.evaluate("document.documentElement.classList.contains('shiny-busy')"):
            time.sleep(0.8)
            return
        time.sleep(0.3)


OUT.mkdir(parents=True, exist_ok=True)
with sync_playwright() as p:
    browser = p.chromium.launch(headless=True)
    ctx = browser.new_context(viewport=VIEW)
    page = ctx.new_page()
    t0 = time.time()
    page.goto(BASE, wait_until="networkidle", timeout=180000)
    page.locator("#sample_textiles").wait_for(state="attached", timeout=180000)
    idle(page)
    print(f"loaded in {time.time() - t0:.1f}s")
    shot(page, "01_home")
    shot(page, "01_home_full", full=True)

    page.locator("#samples_menu summary").click()
    time.sleep(0.5)
    shot(page, "02_samples_menu")
    page.locator("#sample_textiles").click()
    idle(page)
    shot(page, "03_textiles", full=True)

    page.locator("#run_all").click()
    idle(page)
    time.sleep(2)
    shot(page, "04_textiles_saved_run", full=True)

    page.locator("label:has-text('Table')").first.click()
    idle(page)
    shot(page, "05_table", full=True)

    page.locator(".code-summary").click()
    idle(page)
    shot(page, "06_code_open", full=True)

    page.locator("label:has-text('Text')").first.click()
    idle(page)
    shot(page, "07_text_mode", full=True)
    page.locator("#samples_menu summary").click()
    time.sleep(0.4)
    page.locator("#sample_injuries").click()
    idle(page)
    page.locator("#run_all").click()
    idle(page)
    time.sleep(2)
    shot(page, "08_injuries_saved_run", full=True)

    # Change the type to Score and to Yes / no only to see the editors (no run)
    page.locator("label:has-text('Score')").first.click()
    idle(page)
    shot(page, "09_score_editor", full=True)
    page.locator("label:has-text('Yes / no')").first.click()
    idle(page)
    shot(page, "10_noul_editor", full=True)

    page.locator("#tab-about").click()
    time.sleep(1)
    shot(page, "11_about", full=True)
    html = page.content()
    (OUT / "page.html").write_text(html, encoding="utf-8")
    browser.close()
