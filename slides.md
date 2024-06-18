# Slides

I make interactive Jupyter-based slides using RISE for use during class.
You can view them here, or open them to interact.

## Environment

This explains how I configure my environment so you can experiment with the slides locally, or adapt these tools for your own use.

### Install Dependencies

TL;DR:

    pip3 install -r requirements.txt
    jupyter contrib nbextension install --user

And for these (using Julia in Jupyter)

    julia -e 'import Pkg; Pkg.add("IJulia")'

You can open these notebooks as normal notebooks (continuous scroll). If you wish to use them as slides, the key packages are

* [RISE](https://github.com/damianavila/RISE): General editing and presentation of slides
  * Alt-R: Snap in and out of presentation mode
  * Shift-I: toggle slides
  * Shift-G: toggle fragment (incrementally revealed cells)

* [splitcell](https://www.markroepke.me/posts/2019/06/05/tips-for-slideshows-in-jupyter.html): create two columns

    jupyter nbextension enable splitcell/splitcell

  * Shift-S: toggle splitting this cell

* [hide_code](https://github.com/kirbs-/hide_code): Hide code and/or prompts from view

    jupyter nbextension install --py hide_code

  * This is what you want for interactive figures where the code that created the figure is not relevant to the viewer.

### Load the presentation

Open in your browser

    jupyter-nbclassic --notebook-dir=. slides/the-slides.ipynb

Toggle slide-show mode using `Alt-R`.

### Best practices

* When saving, use `Kernel -> Restart & Clear Output` to keep the stored data and diffs in the notebook small.
* PDF files are not web-friendly. PDF images can be converted to SVG using `pdf2svg`, and the result will still look sharp (unlike PNG).
