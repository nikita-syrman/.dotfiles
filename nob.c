#define NOB_IMPLEMENTATION
#include "nob.h"

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    const char *home = getenv("HOME");
    if (!home) {
        nob_log(NOB_ERROR, "HOME not set");
        return 1;
    }

    // Emacs
    nob_log(NOB_INFO, "Installing emacs config...");

    // Create ~/.emacs.d/lisp if needed
    if (!nob_mkdir_if_not_exists(nob_temp_sprintf("%s/.emacs.d", home))) return 1;
    if (!nob_mkdir_if_not_exists(nob_temp_sprintf("%s/.emacs.d/lisp", home))) return 1;

    // Copy .emacs
    nob_copy_file("emacs/.emacs", nob_temp_sprintf("%s/.emacs", home));

    // Copy lisp files
    nob_copy_file("emacs/.emacs.d/lisp/fastc-mode.el",
                  nob_temp_sprintf("%s/.emacs.d/lisp/fastc-mode.el", home));
    nob_copy_file("emacs/.emacs.d/lisp/fasm-mode.el",
                  nob_temp_sprintf("%s/.emacs.d/lisp/fasm-mode.el", home));
    nob_copy_file("emacs/.emacs.d/lisp/casey-theme.el",
                  nob_temp_sprintf("%s/.emacs.d/lisp/casey-theme.el", home));

    nob_log(NOB_INFO, "Done!");
    return 0;
}
