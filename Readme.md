## Readme

Integrate cppcheck with flymake and tramp compatibility (respectin
connection-local variables).

This will try to integrate project and project-multi when a
`compile_commands.json` is available.

At the moment this works for a simple file out of the box, which is
the desired behavior in most of the cases as more complex cases may
use [Eglot](https://github.com/joaotavora/eglot).

However, in remote systems usually there is not clangd, but cppcheck
may be available.
