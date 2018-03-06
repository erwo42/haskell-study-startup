# Haskell Lerngruppe

## Unser Plan für das nächste Treffen am 19.3.2018

Wir wollen am 19.3.2018 ein Coding Dojo veranstalten. Um unsere Kenntnisse im Haskell Ökosystem
weiter auszubauen, ist der Plan, dass sich jeder der Teilnehmer eine der üblichen Containerbibliotheken

1. [Map](https://www.stackage.org/haddock/lts-10.8/containers-0.5.10.2/Data-Map.html)
1. [Sequence](https://www.stackage.org/haddock/lts-10.8/containers-0.5.10.2/Data-Sequence.html)
1. [Set](https://www.stackage.org/haddock/lts-10.8/containers-0.5.10.2/Data-Set.html)
1. [Vector](https://www.stackage.org/haddock/lts-10.8/vector-0.12.0.1/Data-Vector.html)

anschaut und versucht diese Bibliothek bei der Lösung der Aufgabe im Coding Dojo zu verwenden.

Wir werden uns treffen und nach Bekanntgabe der Aufgabe, was erst an dem Abend stattfinden
wird, etwa 90 Minuten arbeiten. Danach führt jeder/jede Gruppe vor, was sie erarbeitet hat
und demonstriert anhand dessen, die Bibliothek, für die sie sich entschieden haben.

Es ist klar, dass je nach Aufgabe, manche Bibliotheken gut oder weniger gut geeignet sein
werden, die Aufgabe zu lösen.

## Weitere mögliche Themen/Vorschläge

1. parser combinators
  * [_Haskell Programming from First Principles_](http://haskellbook.com/) verwendet hier [trifecta](https://www.stackage.org/lts-10.8/package/trifecta-1.7.1.1) was leider nicht so eine gute Dokumentation hat.
  * [megaparsec](https://www.stackage.org/lts-10.8/package/megaparsec-6.3.0) scheint gerade das Mittel der Wahl zu sein (wurde im IRC (freenode, #haskell) und in [FPChat](http://fpchat-invite.herokuapp.com/) empfohlen).
  * [attoparsec](https://www.stackage.org/lts-10.8/package/attoparsec-0.13.2.2) wird wohl verwendet, um binäre Protokolle zu parsen und soll im Vergleich mit die beste Performance (aber die schlechtesten Fehlermeldungen) bieten.
  * Ein Teilnehmer (F.) hat angeboten, wenn wir dieses Thema anhand einer konkreten Problemstellung behandeln, dass er im Vergleich dazu einen Parsergenerator verwenden würde und uns das vorführen würde, damit wir einen Vergleich haben. Hier geht es darum, für das entsprechende Format eine Grammatik zu formulieren und mit den generierten Parser mit dem individuellen Code irgendwie zu erweitern.

1. Vortrag: [Dino Rush](https://github.com/jxv/dino-rush)
Einer arbeitet sich in dieses Projekt ein und ermittelt Teile/Prinzipien, die zur Vorstellung in einem Vortrag vor den anderen geeignet wären. Damit würden wir Einblick in ein reales Projekt gewinnen. Vielleicht kann man hier sehen, wie Architektur im Rahmen funktionaler Programmierung funktionieren kann.

1. Gewinnung eines Sprechers mit Haskell Berufserfahrung
Uns geht es darum, jemanden aus dem Nähkästchen plaudern zu lassen, der Berufserfahrung mit Haskell gesammelt hat und erzählen kann, was gut funktioniert, was weniger gut funktioniert und was seiner Meinung nach die wichtigsten Mittel für den professionellen Einsatz von Haskell sind.

1. Buchkapitel: Foldable, Traversable

1. Buchkapitel: Reader, State, Composing Types, Monad Transformers
Wir könnten das entlang einer konkreten Aufgabe erarbeiten. Als Vorbereitung wären die Kapitel Reader und State durchzuarbeiten. Einer von uns könnte dann einen kurzen einführenden Vortrag halten, der kurz skizziert, was Sinn und Zweck von Monadentransformern sind. Danach könnten wir StateT und ReaderT als Arbeitseinlage vor Ort selbst implementieren, um dann StateT und/ReaderT bei der Lösung einer kleinen vorgegebenen Aufgabe zu verwenden.

1. Buchkapitel: IO

1. Buchkapitel: When things go wrong
Hier würde ich vorschlagen, sich an das bisherige Format zu halten. Jeder arbeitet das Kapitel vor dem Treffen durch und wir besprechen das in Paaren und nachher im Plenum. Ferner kann man ein wenig recherche betreiben, was die empfohlenen Mittel zur Ausnahmebehandlung sind und ob sich das noch mit dem Buch deckt.

1. Buchkapitel: Nonstrictness

1. Themen aus [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html)
Welches Format würde man verwenden? Gemeinsamen Durcharbeiten oder einer macht eine Buchzusammenfassung und skizziert die Möglichkeiten, die Haskell hier bietet?

1. Papers von Simon Peyton Jones [Veröffentlichungen](https://www.microsoft.com/en-us/research/people/simonpj/?from=http%3A%2F%2Fresearch.microsoft.com%2F~simonpj%2Fpapers%2Fhmap%2Findex.htm) zum Thema Haskell durchgehen, zum Beispiel die _Scrap you boilerplate_ Reihe.

## Teilnahme

Folgende Dinge empfehlen wir jedem, der an unseren Treffen teilnehmen will:

- **Tritt dem Meetup der [Karlsruhe Haskell Lerngruppe](https://www.meetup.com/de-DE/preview/Karlsruhe-Haskell-Lerngruppe).**
Termine werden hier angekündigt. Je nachdem, wieviel interesse besteht, könnten wir recht bald anfangen. Spätestens jedoch Januar 2018.

- **Tritt der [FPChat](http://fpchat-invite.herokuapp.com/) Slack Gemeinschaft bei.**
Alle Ankündigungen, die diese Lerngruppe betreffen, werden per Slack oder Meetup verschickt. Such im FPChat Slack nach dem `#karlsruhehaskellstudy` Kanal.

- **[Kaufe](https://gumroad.com/l/haskellbook) deine eigene Ausgabe des "Haskell Book".**

- **Installiere [Stack](https://docs.haskellstack.org/en/stable/README/), das Entwicklungswerkzeug für Haskellprojekte.**
Wenn Du mit andere Packetmanagern, wie npm, pip oder apt vertraut bist, wirst Du merken, dass Stack ganz ähnlich ist. Du musst kein Experte in Stack sein, um zu den Treffen zu erscheinen, aber versuche wenigstens [die Grundlagen zu lernen](resources/haskell-stack-notes.md). Wenn Du schon alles über Stack wissen willst, dann könntest [dieses Video](https://www.youtube.com/watch?v=sRonIB8ZStw) anschauen. Das Allerwichtigste ist, egal was Du tust, _installiere nicht die "Haskell Platform"_.

- **Richte ein Projekt ein, um die Übungen erledigen zu können und mach Dich mit GHC und GHCi vertraut.**
Mehr als ein Verzeichnis für deine Quellen anzulegen und sicher zu gehen, dass Du weißt wie man mit GHC eine Quelle übersetzt oder wie man GHCi startet, um eine REPL zu verwenden, ist nicht notwendig. Ich verwende gerne verschiedene Unterverzeichnisse für die einzelnen Kapitel, aber mach einfach, was Du am bequemsten findest. Wir unternehmen alles, damit es bei unserern Treffen WLAN gibt, aber erwarte nicht, dass es funktioniert.

### Erwartungen

Von Teilnehmern der Lerngruppe wird erwartet, Verantwortung für ihren eignen Lernfortschritt zu übernehmen. Das bedeutet, dass jeder die Regeln einer unterstützenden und gründlichen Lernatmosphäre einhält. Wir erwarten von Teilnehmern, dass sie die jeweiligen vorbereitenden Arbeiten für das nächste Treffen, an dem sie teilnehmen wollen, erledigen. Wenn wir zum Beispiel ein Buchkapitel besprechen, dann ist das:

- Den entsprechenden Teil des Buches lesen.
- All den Code eintippen.
- Alle Aufgaben versuchen.
- Dich mit einem Lernpartner treffen, um Deine Arbeit zu diskutieren.
- Das Gruppentreffen besuchen.

Diese Erwartungen sind nicht dazu da, Dir Dein Leben mies zu machen, sondern um Dir einen Rahmen für Deinen Erfolg zu geben. Wenn Du wirklich Haskell lernen willst: Konsequenz ist der Schlüssel dazu -- wie bei allem. Da wir Haskell in dieser Gruppe zusammen lernen werden, ist es wichtig, dass Du zuerst jedes Kapitel für Dich selbst durcharbeitest, sodass Du vorbreitet und in dem Wissen, wo Du Hilfe brauchst und was Du gut genug verstanden hast, um es anderen beizubringen, zum Gruppentreffen kommst. Wenn Du bei einer Übungsaufgabe stecken bleibst, geh zur Nächsten über und überspringe nicht einfach alle Aufgaben.
_**Nur um sicher zu gehen: Überspringe die Übungsaufgaben nicht!**_

Hier der Ablauf eines Treffens, dass der Besprechung eines Buchkapitels dient: [Agenda](agenda.md)


### Verhaltensregeln

Es wird von Teilnehmern erwartet, dass sie ihr bestes geben, anständig und respektvoll miteinander umzugehen. Wessen Verhalten sich zu oft zu weit von den akzeptablen Grenzen des respektvollen, freundlichen und kollegialen Umgangs miteinander entfernt, wird ausgeschlossen.

Das [Recurse Center](https://www.recurse.com) hat eine Sammlung brauchbarer [sozialer Regeln](https://www.recurse.com/manual#sub-sec-social-rules), die auch bei unseren Treffen gelten.

## Folien und andere Materialien

Im [Materialien](resources) Unterverzeichnis diese Repositorys findest Du Folien, Zusatzaufgaben und andere Materialien, die wir während der Treffen verwenden können. Wenn Du etwas hinzufügen willst, mach einen pull request.

## Rückblick

**Woche 1. Einführung. Lambdakalkül.**
- Haskell Book, Kapitel 1

**Woche 2. Einstieg in Haskell.**
- Kapitel 2 und 3

**Woche 3, Grundlegende Datentypen.**
- Kapitel 2 und 3 nochmal durchsehen
- Kapitel 4 und 5

**Woche 4. Typen und Typklassen.**
- Kapitel 4 und 5 nochmal durchsehen
- Kapitel 6

**Woche 5. Funktionale Muster. Arbeiten mit Rekursion.**
- Kapitel 6 nochmal durchsehen
- Kapitel 7 und 8

**Woche 6. Listen und Faltungen von Listen.**
- Kapitel 7 und 8 nochmal durchsehen
- Kapitel 9 und 10
- Wir haben hier bereits Kapitel 11 und 12 eingeschoben,
  sodass wir die restlichen Termine aufrücken können.

**Woche 7. Rückblick zur Halbzeit. Testen mit QuickCheck.**
- Kapitel 2 bis 12 nochmal durchsehen
- Kapitel 13 (ohne Besprechung in der Gruppe)
- Kapitel 14

**Woche 8. Monoide und Halbgruppen.**
- Kapitel 15

**Woche 9. Funktoren.**
- Kapitel 15 nochmal durchsehen
- Kapitel 16

**Woche 10. Applicative.**
- Kapitel 16 nochmal durchsehen
- Kapitel 17

**Woche 11. Monaden. Wie man Strukturen auf Code anwendet.**
- Kapitel 17 nochmal durchsehen
- Kapitel 18
- Kapitel 19 (ohne Besprechung in der Gruppe**
- Allgemeiner Rückblick und Ausblick

**Woche 12. Noch offen, Themenvorschläge sind willkommen!**
