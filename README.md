# scrapekicker

In diesem Repo werden Daten zur aktuellen Bundesligasaison von kicker.de gelesen und gespeichert. Der Fokus liegt auf relevanten Informationen für das kicker Managerspiel Interactive.

Für jedes Spiel wird gespeichert, welche Spieler eingesetzt wurden oder auf der Bank saßen und ihre für das Managerspiel relevanten Datenpunkte (Tore, Vorlagen, Noten etc).

## Skriptdateien

In "scrape_season.R" werden für jeden Spieltag der Saison 24/25 alle relevanten Daten aus den jeweiligen Spielseiten des kickers gezogen und im Ordner "data" gespeichert. 
Zusätzlich wird ein Datensatz für die gesamte Saison namens "players_full" gespeichert.

In "enhance_interactive.R" werden die speziell zum Managerspiel Interactive gehörenden Informationen (insbesondere Positionszuteilung und Preis der einzelnen Spieler) im Datensatz "players_interactive.xlsx" gespeichert.
Einzelne Fehler (z.B. bei gleichen Nachnamen) wurden manuell ausgebessert, der verbesserte Datensatz unter "players_interactive_edit.xlsx" gespeichert.
Zuletzt werden die Datensätze zu Spieltagen und Interactive zusammengefügt. Danach werden für jeden Spieler seine erzielten Punkte pro Spiel berechnet und zuletzt mit der vom kicker berechneten Punktzahl verglichen, um Unterschiede festzustellen und zu verbessern.

## Hauptdatensatz

Der fertige Hauptdatensatz heißt "players" und liegt in drei Versionen vor: 
- als .csv-Datei: https://github.com/false-10/scrapekicker/blob/main/data/2425/players.csv
- als .xlsx-Datei: https://github.com/false-10/scrapekicker/blob/main/data/2425/players.xlsx
- als .RDS-Datei: https://github.com/false-10/scrapekicker/blob/main/data/2425/players.RDS

Jede Beobachtung/Reihe entspricht den Datenpunkten eines Spielers in einem Spiel. 
Es gibt für jedes Spiel jeweils eine Beobachtung/Reihe pro Spieler, der sich mindestens im Kader befindet.

Folgende Informationen sind darin enthalten:

Basisinformationen zum Spiel (Spieltag, Vereine, Ergebnis, Austragungsort).
Einsatzinformationen zum Spieler (Startelf/Einwechslung/Bankplatz, Einsatzzeiten, Tore, Vorlagen, Karten).
kicker-spezifische Informationen (Note, Spieler des Spiels).
Managerspiel-spezifische Informationen (Positionszuteilung, Marktwert; Punkte für Tore, Vorlagen, Note usw).

Im Detail:

- **spt**: Spieltag
- **team**: Verein des Spielers
- **status**: Einsatzart des Spielers
  - "start" für einen Platz in der Startelf
  - "sub" für eine Einwechslung
  - "bench" für eine Listung im Kader ohne Einsatz
- **player**: (Angezeigter) Name des Spielers
- **begin**: erste Einsatzminute des Spielers
  - "0" für Startelfspieler
  - z.B. "60" für einen in der 61. Minute eingewechselten Spieler
- **end**: letzte Einsatzminute des Spielers
  - "90" für Spieler, die durchspielen oder in der Nachspielzeit ausgewechselt werden
  - z.B. "80" für einen in der 81. Minute ausgewechselten Spieler
- **grade**: Note des Spielers
- **location**: Ort des Spiels
  - "home" für ein Heimspiel des Spielers
  - "away" für ein Auswärtsspiel des Spielers
- **opponent**: Gegnerverein des Spielers
- **tG**: *team goals*, also erzielte Tore der Mannschaft des Spielers
- **tGA**: *team goals against*, also kassierte Tore der Mannschaft des Spielers
- **npG**: *non-penalty goals*, erzielte Tore des Spielers (ohne Elfmeter)
- **pG**: *penalty goals*, erzielte Elfmeter des Spielers
- **npA**: *non-penaly assists*, Vorlagen des Spielers ohne Vorlagen für Elfmeter
- **pA**: *penalty assists*, Vorlagen für Elfmeter des Spielers
- **ownG**: *own goals*, Eigentore des Spielers
- **ylw**: *yellow*, gelbe Karten des Spielers
- **ylwred**: *yellow-red*, gelbrote Karten des Spielers
- **red**: *red*, rote Karten des Spielers
- **sds**: *Spieler des Spiels*, Auszeichnung als Spieler des Spiels
- **Position**: Positionszuteilung des Spielers im Managerspiel
  - "GOALKEEPER" für Torwarte
  - "DEFENDER" für Verteidiger
  - "MIDFIELDER" für Mittelfeldspieler
  - "FORWARD" für Stürmer
- **kID**: kicker-ID des Spielers
- **MW**: Marktwert des Spieler im Managerspiel
- **Punkte**: Gesamtpunktzahl des Spielers im Managerspiel laut kicker
- **Note**: Gesamtdurchschnittsnote des Spielers laut kicker
  - "0.00" für Spieler ohne Note
- **G_coef**: *goal coefficient*, Torpunktkoeffizient des Spielers im Managerspiel
  - "6" für Torwarte
  - "5" für Verteidiger
  - "4" für Mittelfeldspieler
  - "3" für Stürmer
- **status_pts**: Punkte für den Status des Spielers
  - "4" für einen Startelfeinsatz
  - "2" für eine Einwechslung
  - "0" für keinen Einsatz
- **grade_pts**: Punkte für die Note des Spielers
  - Berechnung: (3.5-Note)*4
- **npG_pts**: Punkte für erzielte Tore des Spielers ohne Elfmeter
- **pG_points**: Punkte für erzielte Elfmetertore des Spielers
- **npA_pts**: Punkte für Vorlagen zu Toren ohne Elfmeter
- **pA**: Punkte für Vorlagen zu Elfmetertoren
- **ylwred_pts**: Punkte für eine mögliche gelbrote Karte des Spielers
- **red_pts**: Punkte für eine mögliche rote Karte des Spielers
- **sds_pts**: Punkte für eine mögliche Auszeichnung als Spieler des Spiels
- **clean_sheet_pts**: Punkte für eine möglichen Zu-Null-Bonus von Torwarten
- **points**: errechnete Punkte eines Spielers im Spiel
