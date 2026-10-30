"""Render the cards of the demo video (1920 x 864, the top part of the frame).

Miami brand (D281): a Miami Red field, warm white text, no tints. Fonts are
the fonts of the app (assets/fonts, SIL Open Font License): Source Serif 4
for titles, Source Sans 3 for text, IBM Plex Mono for labels.

Run from video/:  python make_cards.py   ->  cards/<name>.png
"""
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).resolve().parent
FONTS = HERE / "assets" / "fonts"
CARDS = HERE / "cards"
LOGO = HERE / "assets" / "brand" / "decisions_logo.png"

W, H = 1920, 864
RED = (196, 18, 48)          # Miami Red #C41230
WARM = (250, 249, 247)       # Warm White #FAF9F7 (5.9:1 on Miami Red)
WHITE = (255, 255, 255)

SERIF = FONTS / "source-serif-4-latin-700-normal.woff2"
SANS = FONTS / "source-sans-3-latin-400-normal.woff2"
SANS_SB = FONTS / "source-sans-3-latin-600-normal.woff2"
SANS_BD = FONTS / "source-sans-3-latin-700-normal.woff2"
MONO = FONTS / "ibm-plex-mono-latin-600-normal.woff2"
MONO_RG = FONTS / "ibm-plex-mono-latin-400-normal.woff2"


def font(path, size):
    return ImageFont.truetype(str(path), size)


def new_card():
    img = Image.new("RGB", (W, H), RED)
    return img, ImageDraw.Draw(img)


def tracked(d, xy, text, f, fill, tracking=4, anchor="l"):
    """Letter-spaced text (labels). anchor "l" starts at x, "m" centres on x."""
    widths = [d.textlength(c, font=f) for c in text]
    total = sum(widths) + tracking * (len(text) - 1)
    x, y = xy
    if anchor == "m":
        x -= total / 2
    for c, w in zip(text, widths):
        d.text((x, y), c, font=f, fill=fill)
        x += w + tracking
    return total


def wrap(d, text, f, max_w):
    lines, cur = [], ""
    for word in text.split():
        test = f"{cur} {word}".strip()
        if d.textlength(test, font=f) <= max_w:
            cur = test
        else:
            lines.append(cur)
            cur = word
    lines.append(cur)
    return lines


def ctext(d, y, text, f, fill):
    d.text((W / 2, y), text, font=f, fill=fill, anchor="ma")


def logo(img, cx, cy, height):
    mark = Image.open(LOGO).convert("RGBA")
    w = round(mark.width * height / mark.height)
    mark = mark.resize((w, height), Image.LANCZOS)
    img.paste(mark, (round(cx - w / 2), round(cy - height / 2)), mark)


def save(img, name):
    CARDS.mkdir(exist_ok=True)
    img.save(CARDS / f"{name}.png")
    print("card", name)


def intro():
    img, d = new_card()
    # The hex logo sits on a warm white disc, so that its red reads on the red field
    d.ellipse((W / 2 - 112, 120, W / 2 + 112, 344), fill=WARM)
    logo(img, W / 2, 232, 160)
    ctext(d, 380, "decisions", font(SERIF, 112), WARM)
    ctext(d, 530, "Ask a decision model a question about each image or text", font(SANS_SB, 46), WARM)
    tracked(d, (W / 2, 640), "A FREE APP ON HUGGING FACE", font(MONO, 26), WARM, 5, "m")
    ctext(d, 690, "huggingface.co/spaces/fmegahed/decisions", font(MONO_RG, 30), WARM)
    save(img, "intro")


def types():
    img, d = new_card()
    tracked(d, (W / 2, 92), "QUESTION TYPES", font(MONO, 28), WARM, 6, "m")
    ctext(d, 140, "Three types of question", font(SERIF, 72), WARM)
    cols = [
        ("Yes / no", "question_noul()", ["Two fixed answers:", "true and false"]),
        ("Choice", "question_choice()", ["Two to six answers", "that you name"]),
        ("Score", "question_score()", ["Two to five levels,", "from low to high"]),
    ]
    cw, gap, top = 520, 50, 300
    x0 = (W - (3 * cw + 2 * gap)) / 2
    for i, (name, fn, body) in enumerate(cols):
        x = x0 + i * (cw + gap)
        d.rounded_rectangle((x, top, x + cw, top + 330), radius=22, outline=WARM, width=3)
        d.text((x + cw / 2, top + 44), name, font=font(SERIF, 58), fill=WARM, anchor="ma")
        d.text((x + cw / 2, top + 132), fn, font=font(MONO_RG, 30), fill=WARM, anchor="ma")
        for j, line in enumerate(body):
            d.text((x + cw / 2, top + 206 + j * 48), line, font=font(SANS, 38), fill=WARM, anchor="ma")
    ctext(d, 700, "For each input, Jev gives a probability for every answer.", font(SANS_SB, 40), WARM)
    save(img, "types")


def section(name, num, title, sub):
    img, d = new_card()
    tracked(d, (W / 2, 250), f"PART {num}", font(MONO, 34), WARM, 8, "m")
    ctext(d, 310, title, font(SERIF, 110), WARM)
    d.line((W / 2 - 90, 500, W / 2 + 90, 500), fill=WARM, width=4)
    ctext(d, 536, sub, font(SANS_SB, 44), WARM)
    save(img, name)


def section_images(name, num, title, images, credit):
    """A section card with example images: [(path, label)]."""
    img, d = new_card()
    tracked(d, (W / 2, 70), f"PART {num}", font(MONO, 30), WARM, 8, "m")
    ctext(d, 116, title, font(SERIF, 88), WARM)
    side, gap, top = 380, 90, 268
    x0 = (W - (len(images) * side + (len(images) - 1) * gap)) / 2
    for i, (path, label) in enumerate(images):
        x = round(x0 + i * (side + gap))
        pic = Image.open(path).convert("RGB").resize((side, side), Image.LANCZOS)
        d.rectangle((x - 6, top - 6, x + side + 6, top + side + 6), fill=WARM)
        img.paste(pic, (x, top))
        d.text((x + side / 2, top + side + 30), label, font=font(SANS_SB, 38), fill=WARM, anchor="ma")
    ctext(d, 790, credit, font(SANS, 28), WARM)
    save(img, name)


def recap():
    img, d = new_card()
    tracked(d, (W / 2, 110), "SUMMARY", font(MONO, 28), WARM, 6, "m")
    ctext(d, 158, "Four steps", font(SERIF, 72), WARM)
    items = ["Bring images or texts: a sample, a zip file, pasted text or a CSV file",
             "Ask a Yes / no, Choice or Score question, with references for images",
             "Read the answers and the raw probabilities of Jev",
             "Take the R code and the bundle to your own R"]
    f_num, f_txt = font(MONO, 34), font(SANS_SB, 42)
    y = 320
    for i, text in enumerate(items, start=1):
        d.ellipse((300, y - 4, 360, y + 56), fill=WARM)
        d.text((330, y + 26), str(i), font=f_num, fill=RED, anchor="mm")
        d.text((396, y + 2), text, font=f_txt, fill=WARM)
        y += 112
    save(img, "recap")


def outro():
    img, d = new_card()
    tracked(d, (W / 2, 96), "TRY IT", font(MONO, 28), WARM, 6, "m")
    ctext(d, 142, "huggingface.co/spaces/fmegahed/decisions", font(SERIF, 64), WARM)
    ctext(d, 262, "R package: github.com/fmegahed/decisions", font(SANS_SB, 40), WARM)
    ctext(d, 362, "Fadel M. Megahed, Miami University", font(SANS_SB, 42), WARM)
    ctext(d, 418, "Ying-Ju (Tessa) Chen, University of Dayton", font(SANS_SB, 42), WARM)
    d.line((W / 2 - 90, 512, W / 2 + 90, 512), fill=WARM, width=3)
    tracked(d, (W / 2, 548), "CREDITS", font(MONO, 24), WARM, 6, "m")
    credits = [
        "Woven textiles: simulated with the R package spc4sts (github.com/fmegahed/qe_genai, MIT)",
        "Brake images: AutoVI (Carvalho et al., 2024), CC BY-NC-SA 4.0",
        "Injury reports: OSHA Severe Injury Reports, public domain",
        "Model: Jev (jev-1.14.0) from TypeSafe  ·  Voice: ElevenLabs",
    ]
    for i, line in enumerate(credits):
        ctext(d, 596 + i * 46, line, font(SANS, 32), WARM)
    save(img, "outro")


if __name__ == "__main__":
    intro()
    types()
    section("part1", 1, "Images: a sample", "Woven textiles · a Choice question · a saved run")
    refs = HERE / "assets" / "data" / "references"
    section_images("part2", 2, "Images: your own data", [
        (refs / "false" / "good_0035.png", "The staple is there"),
        (refs / "true" / "missing_0035.png", "The staple is missing"),
    ], "Brake assemblies from AutoVI (Carvalho et al., 2024), CC BY-NC-SA 4.0")
    section("part3", 3, "Text", "Injury reports · a Choice question, then a Score question")
    recap()
    outro()
