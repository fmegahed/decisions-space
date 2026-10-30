# decisions Space demo: narration script

Voice: ElevenLabs (D282), set in make_vo.py. One beat for each shot.
gen_timeline.py fits the picture to the duration of each beat.

Markup: `{shown|spoken}` shows the first text in the narration strip and gives
the second text to the voice (for example `{AUC|A U C}`). Beat lines have the
form `**vNN_name** — text`; other lines are notes.

## Part 0: intro

**v00_intro** — This is decisions, a free app on Hugging Face. It asks a decision model, Jev from TypeSafe, one question about each image or text. It gives you the answers, their probabilities, and the R code of the run.

**v01_types** — Each run starts with a question of one of three types. Yes or no, which the R package calls {noul|nool}, has two fixed answers: true and false. Choice has two to six answers that you name. Score has two to five levels, in order from low to high. For each input, Jev gives a probability for every answer.

## Part 1: images, a sample

**v10_card** — Part one: images, with a sample.

**v11_layout** — On the left, three groups: the data, the question, and the references. On the right, the answers and the R code. The Help switch shows or hides the help notes.

**v12_sample** — Open Samples and pick Woven textiles: forty-six simulated textile images, each with a label.

**v13_question** — The question is a Choice. The instructions ask if the weave has a flaw. Each answer, nominal and defective, has a short description. The references give Jev two example images for each answer.

**v14_run** — Try on 5 asks about the first five images. Run all asks about all of them, and shows the cost before you click. A sample shows its saved run, at no cost.

**v15_answers** — The answers card shows the {AUC|A U C}, the accuracy, the number of images, and the cost. Each tile shows the probability of the target answer, here defective, with the answer of Jev and the label. The highest probabilities come first.

**v16_table** — The table gives the probability of every answer, with the tokens and the seconds of each call. Answers {.csv|dot C S V} downloads it.

**v17_code** — The R code card repeats the run in R: the question, the references, and one call to decide. Copy the code, download the R file, or download the bundle, which also holds the setup and the answers.

**v18_import** — Import reads a bundle back into the app.

## Part 2: images, your own data

**v20_card** — Part two: your own images. These come from our paper: brake assemblies from the {Automotive Visual Inspection data set (AutoVI)|Automotive Visual Inspection data set}, where a metal staple holds a pipe. In some parts, the staple is missing.

**v21_noul** — So we ask a Yes or no question: is the staple missing? We pick Yes or no, and type the instructions.

**v22_desc** — Yes or no has the fixed answers true and false. Each answer gets a description. These descriptions come from the paper.

**v23_zip** — The zip file has one folder for each answer, true and false, with eight images in each, so the app knows the labels. Then we add two reference images for each answer.

**v24_try** — Try on 5 checks the setup on five images first. Here, all five answers are right. The meter below the buttons shows what this visit has spent.

**v25_runall** — Run all asks about all sixteen images, for less than one cent. Every missing staple ranks above every good part: the {AUC|A U C} and the accuracy are one.

**v26_calib** — But look at the probabilities: the missing staples get only {0.56 to 0.67|zero point five six to zero point six seven}. These are raw probabilities, not calibrated. The R package can calibrate them, as our paper does.

**v27_code** — For your own images, the R code reads the images from relative paths, and the bundle holds the references.

## Part 3: text

**v30_card** — Part three: text.

**v31_sample** — For text, switch the data to Text. The injury reports sample asks a Choice question with five injury types, on seventy-five reports of work injuries. Its saved run is right ninety-six percent of the time.

**v32_paste** — With your own data, paste the texts, with a blank line between two texts, or load a {CSV|C S V} file and pick the text column. Here: six short injury reports.

**v33_score** — Now a Score question: how severe is the injury? A score has levels in order, from low to high. We add a third level and describe each one: low, medium, and high. A score question takes no references.

**v34_run** — Run all. For each report, Jev gives a probability for each level, and the answer is the most likely level. The target is high, so the amputations come first.

## Recap and outro

**v50_recap** — So: bring images or texts, ask a Yes or no, Choice, or Score question, read the probabilities, and take the R code to your own computer.

**v51_outro** — Try it at {huggingface.co/spaces/fmegahed/decisions|hugging face dot co, slash spaces, slash F Megahed, slash decisions}. Made by Fadel Megahed at Miami University and Ying-Ju Tessa Chen at the University of Dayton.
