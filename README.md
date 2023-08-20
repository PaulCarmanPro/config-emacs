# emacs-configuration

### Goal
Create a non-modal keyboard where every key functionally does the same thing all the time.

### Issues
ESC should always to perform the action of Control-G. The declarations I have now are very inadequate. There is no code which examines every keymap (except man-mode-map) and changes Control-G to Escape.

### Comarison to Vim
Emacs and Vim are complete opposites. Loving speed, I initially chose to use Vim. I liked Vim, but as my tallents grew I started creating macros and found the basic curcsor movment keys which every macro must be built upon to be annoying. The very thing that gives Vim its speed, the immutable foundation, was clearly flawed and I didn't want to work with it. So I switched to Emacs. Emacs is incredibly huge and slow, but everything about it is mutable and I was either able to cause Emacs to do what I wanted or believed that I could cause Emacs to do what I wanted.
