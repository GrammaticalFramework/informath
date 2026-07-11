# Forthcoming Informath languages

This directory contains experimental grammars that have been produced by using techniques such as vibe coding and not yet properly checked. They can be compiled by the
```
make next_grammars
```
on the top level of Informath; you can edit the `Makefile` entry to contain just the languages you want. Then you can test, for instance,
```
make lang=Cze top100
```
The first examples are Finnish and Czech, which were produced during one day with Claude code. The Claude-generated [documentation](../../doc/vibe/) can guide you add yet another language. The case of Finnish was easier, because the RGL was complete for the task, whereas in Czech, also parts of the RGL were built or completed in the process.

