######################################################
#                                                    #
#  standard.ld                  Jeffrey H. Kingston  #
#                                     February 1995  #
#                                     February 1999  #
#                                                    #
#  Database of commonly used symbols:                #
#                                                    #
#    @Word           language-specific words         #
#    @Roman          lower case Roman numerals       #
#    @UCRoman        upper case Roman numerals       #
#    @Alpha          lower case Roman alphabet       #
#    @UCAlpha        upper case Roman alphabet       #
#    @Months         months of the year              #
#    @ShortMonths    months of the year, abbreviated #
#    @WeekDays       days of the week                #
#    @ShortWeekDays  days of the week, abbreviated   #
#    @TwelveHours    hours on twelve-hour clock      #
#    @ShortHours     short hours on 24-hour clock    #
#    @MeriDiem       a.m. or p.m.                    #
#    @ShortMeriDiem  am or pm                        #
#    @DateTimeFormat format of date and time.        #
#                                                    #
######################################################

{ contents @Word @CurrLang @Case {
    Croatian	@Yield { Sadr{@Char zcaron}aj }
    Czech	@Yield { Obsah }
    Danish	@Yield { Indhold }
    Dutch       @Yield { Inhoudsopgave }
    English	@Yield { Contents }
    EnglishUK	@Yield { Contents }
    Finnish	@Yield { Sis{@Char adieresis}lt{@Char odieresis} }
    French	@Yield { Table des Mati{@Char egrave}res }
    German	@Yield { Inhalt }
    Hungarian	@Yield { Tartalom }
    Italian	@Yield { Indice }
    Norwegian   @Yield { Innhold }
    Polish	@Yield { Spis tre{@Char sacute}ci }
    Portuguese  @Yield { Conte{@Char uacute}do }
    Russian	@Yield { "\363\317\304\305\322\326\301\316\311\305" }
    Slovak	@Yield { Obsah }
    Slovenian   @Yield { Vsebina }
    Spanish	@Yield { Contenidos }
    Swedish	@Yield { Inneh{@Char aring}ll }
    UpperSorbian @Yield { Wobsah }
  }
}

{ references @Word @CurrLang @Case {
    Croatian	@Yield { Reference }
    Czech	@Yield { Odkazy na literaturu }
    Danish	@Yield { Referencer }
    Dutch       @Yield { Referenties }
    English	@Yield { References }
    EnglishUK	@Yield { References }
    Finnish	@Yield { Viitteet }
    French	@Yield { R{@Char eacute}f{@Char eacute}rences }
    German	@Yield { Literaturverzeichnis }
    Hungarian	@Yield { Hivatkoz{@Char aacute}sok }
    Italian	@Yield { Riferimenti }
    Norwegian   @Yield { Referanser }
    Polish	@Yield { Odno{@Char sacute}niki }
    Portuguese  @Yield { Refer{@Char ecircumflex}ncias }
    Russian	@Yield { "\354\311\324\305\322\301\324\325\322\301" }
    Slovak	@Yield { Odkazy na literat{@Char uacute}uru }
    Slovenian   @Yield { Reference }
    Spanish	@Yield { Referencias }
    Swedish	@Yield { Referenser }
    UpperSorbian @Yield { Literatura }
  }
}

{ bibliography @Word @CurrLang @Case {
    Croatian	@Yield { Bibliografija }
    Czech	@Yield { Seznam literatury }
    Danish	@Yield { Bibliografi }
    Dutch       @Yield { Bibliografie } 
    English	@Yield { Bibliography }
    EnglishUK	@Yield { Bibliography }
    Finnish	@Yield { Kirjallisuusluettelo }
    French	@Yield { Bibliographie }
    German	@Yield { Literaturverzeichnis }
    Hungarian	@Yield { Irodalomjegyz{@Char eacute}k }
    Italian	@Yield { Bibliografia }
    Norwegian   @Yield { Litteratur }
    Polish	@Yield { Literatura }
    Portuguese  @Yield { Bibliografia }
    Russian	@Yield { "\354\311\324\305\322\301\324\325\322\301" }
    Slovak	@Yield { Zoznam literat{@Char uacute}ry }
    Slovenian   @Yield { Literatura }
    Spanish	@Yield { Bibliograf{@Char iacute}a }
    Swedish	@Yield { Bibliografi }
    UpperSorbian @Yield { Bibliografija }
  }
}

{ figure @Word @CurrLang @Case {
    Croatian	@Yield { Slika }
    Czech	@Yield { Obr{@Char aacute}zek }
    Danish	@Yield { Figur }
    Dutch       @Yield { Figuur }
    English	@Yield { Figure }
    EnglishUK	@Yield { Figure }
    Finnish	@Yield { Kuva }
    French	@Yield { Figure }
    German	@Yield { Abbildung }
    Hungarian	@Yield { {@Char aacute}bra }
    Italian	@Yield { Figura }
    Norwegian   @Yield { Figur  }
    Polish	@Yield { Rysunek }
    Portuguese  @Yield { Figura }
    Russian	@Yield { "\362\311\323\325\316\317\313" }
    Slovak	@Yield { Obr{@Char aacute}zok }
    Slovenian   @Yield { Slika }
    Spanish	@Yield { Figura }
    Swedish	@Yield { Figur }
    UpperSorbian @Yield { Wobraz }
  }
}

{ figurelist @Word @CurrLang @Case {
    Croatian	@Yield { Popis slika }
    Czech	@Yield { Seznam obr{@Char aacute}zk{@Char uring} }
    Danish	@Yield { Figurliste }
    Dutch       @Yield { Lijst van figuren } # Figuren, Figurenlijst
    English	@Yield { List of Figures }
    EnglishUK	@Yield { List of Figures }
    Finnish	@Yield { Kuvat }
    French	@Yield { Liste des figures }
    German	@Yield { Abbildungsverzeichnis }
    Hungarian	@Yield { {@Char Aacute}br{@Char aacute}k jegyz{@Char eacute}ke }
    Italian	@Yield { Elenco delle figure }
    Norwegian   @Yield { ??  }
    Polish	@Yield { Spis rysunk{@Char oacute}w }
    Portuguese  @Yield { Lista de Figuras }
    Russian	@Yield { "\363\320\311\323\317\313 \311\314\314\300\323\324\322\301\303\311\312" }
    Slovak	@Yield { Zoznam obr{@Char aacute}zkov }
    Slovenian   @Yield { Seznam slik }
    Spanish	@Yield { Lista de figuras }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Zapisk wobrazow }
  }
}

{ table @Word @CurrLang @Case {
    Croatian	@Yield { Tablica }
    Czech	@Yield { Tabulka }
    Danish	@Yield { Tabel }
    Dutch       @Yield { Tabel }
    English	@Yield { Table }
    EnglishUK	@Yield { Table }
    Finnish	@Yield { Taulukko }
    French	@Yield { Table }
    German	@Yield { Tabelle }
    Hungarian	@Yield { t{@Char aacute}bl{@Char aacute}zat }
    Italian	@Yield { Tabella }
    Norwegian   @Yield { Tabell }
    Polish	@Yield { Tabela }
    Portuguese  @Yield { Tabela }
    Russian	@Yield { "\364\301\302\314\311\303\301" }
    Slovak	@Yield { Tabu{@Char lcaron}ka }
    Slovenian   @Yield { Tabela }
    Spanish	@Yield { Tabla }
    Swedish	@Yield { Tabell }
    UpperSorbian @Yield { Tabulka }
  }
}

{ tablelist @Word @CurrLang @Case {
    Croatian	@Yield { Popis tablica }
    Czech	@Yield { Seznam tabulek }
    Danish	@Yield { Tabelliste }
    Dutch       @Yield { Lijst van tabellen }  # Tabellen, Tabellenlijst
    English	@Yield { List of Tables }
    EnglishUK	@Yield { List of Tables }
    Finnish	@Yield { Taulukot }
    French	@Yield { Liste des tables }
    German	@Yield { Tabellenverzeichnis }
    Hungarian	@Yield { T{@Char aacute}bl{@Char aacute}zatok jegyz{@Char eacute}ke }
    Italian	@Yield { Elenco delle tabelle }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Spis tabel }
    Portuguese  @Yield { Lista de Tabelas }
    Russian	@Yield { "\363\320\311\323\317\313 \324\301\302\314\311\303" }
    Slovak	@Yield { Zoznam tabuliek }
    Slovenian   @Yield { Seznam tabel }
    Spanish	@Yield { Lista de tablas  }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Zapisk tabulkow }
  }
}

{ definition @Word @CurrLang @Case {
    Croatian	@Yield { Definicija }
    Czech	@Yield { Definice }
    Danish	@Yield { Definition }
    Dutch       @Yield { Definitie }
    English	@Yield { Definition }
    EnglishUK	@Yield { Definition }
    Finnish	@Yield { M{@Char adieresis}{@Char adieresis}ritelm{@Char adieresis} }
    French	@Yield { D{@Char eacute}finition }
    German	@Yield { Definition }
    Hungarian	@Yield { defin{@Char iacute}ci{@Char oacute} }
    Italian	@Yield { Definizione }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Definicja }
    Portuguese  @Yield { Defini{@Char ccedila}{@Char atilde}o }
    Russian	@Yield { "\357\320\322\305\304\305\314\305\316\311\305" }
    Slovak	@Yield { Defin{@Char iacute}ce }
    Slovenian   @Yield { Definicija }
    Spanish	@Yield { Definici{@Char oacute}n }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Definicija }
  }
}

{ lemma @Word @CurrLang @Case {
    Croatian	@Yield { Lema }
    Czech	@Yield { Lemma }
    Danish	@Yield { Lemma }
    Dutch       @Yield { Lemma }
    English	@Yield { Lemma }
    EnglishUK	@Yield { Lemma }
    Finnish	@Yield { Lemma }
    French	@Yield { Lemme }
    German	@Yield { Lemma }
    Hungarian	@Yield { lemma }
    Italian	@Yield { Lemma }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Lemat }
    Portuguese  @Yield { Lema }
    Russian	@Yield { "\354\305\315\315\301" }
    Slovak	@Yield { Lemma }
    Slovenian   @Yield { Lema }
    Spanish	@Yield { ?? }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Lema }
  }
}

{ theorem @Word @CurrLang @Case {
    Croatian	@Yield { Teorem }
    Czech	@Yield { Teor{@Char eacute}m }
    Danish	@Yield { S{@Char ae}tning }
    Dutch       @Yield { Theorema }
    English	@Yield { Theorem }
    EnglishUK	@Yield { Theorem }
    Finnish	@Yield { Lause }
    French	@Yield { Th{@Char eacute}or{@Char egrave}me }
    German	@Yield { Theorem }
    Hungarian	@Yield { t{@Char eacute}tel }
    Italian	@Yield { Teorema }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Twierdzenie }
    Portuguese  @Yield { Teorema }
    Russian	@Yield { "\364\305\317\322\305\315\301" }
    Slovak	@Yield { Teor{@Char eacute}ma }
    Slovenian   @Yield { Teorem }
    Spanish	@Yield { Teorema }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Teorem }
  }
}

{ proposition @Word @CurrLang @Case {
    Croatian	@Yield { Propozicija }
    Czech	@Yield { P{@Char rcaron}edpoklad }
    Danish	@Yield { Proposition }
    Dutch       @Yield { Propositie } # Bewering
    English	@Yield { Proposition }
    EnglishUK	@Yield { Proposition }
    Finnish	@Yield { Propositio }
    French	@Yield { Proposition }
    German	@Yield { Satz }
    Hungarian	@Yield { {@Char aacute}ll{@Char iacute}t{@Char aacute}s }
    Italian	@Yield { Proposizione }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Za{@Char lslash}o{@Char zdotabove}enie }
    Portuguese  @Yield { Proposi{@Char ccedilla}{@Char atilde}o }
    Russian	@Yield { "\360\322\305\304\314\317\326\305\316\311\305" }
    Slovak	@Yield { Predpoklad }
    Slovenian   @Yield { Predpostavka }
    Spanish	@Yield { Proposici{@Char oacute}n }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Postajenje }
  }
}

{ corollary @Word @CurrLang @Case {
    Croatian	@Yield { Korolar }
    Czech	@Yield { D{@Char uring}sledek }
    Danish	@Yield { Korollar }
    Dutch       @Yield { Corollaar }
    English	@Yield { Corollary }
    EnglishUK	@Yield { Corollary }
    Finnish	@Yield { Seuraus }
    French	@Yield { Corollaire }
    German	@Yield { Korollar }
    Hungarian	@Yield { ?? }
    Italian	@Yield { Corollario }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Wniosek }
    Portuguese  @Yield { Corol{@Char aacute}rio }
    Russian	@Yield { "\363\314\305\304\323\324\327\311\305" }
    Slovak	@Yield { D{@Char ocircumflex}sledok }
    Slovenian   @Yield { Sklep }
    Spanish	@Yield { Corolario }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Korolar }
  }
}

{ example @Word @CurrLang @Case {
    Croatian	@Yield { Primjer }
    Czech	@Yield { P{@Char rcaron}{@Char iacute}klad }
    Danish	@Yield { Eksempel }
    Dutch       @Yield { Voorbeeld }
    English	@Yield { Example }
    EnglishUK	@Yield { Example }
    Finnish	@Yield { Esimerkki }
    French	@Yield { Exemple }
    German	@Yield { Beispiel }
    Hungarian	@Yield { p{@Char eacute}lda }
    Italian	@Yield { Esempio }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Przyk{@Char lslash}ad }
    Portuguese  @Yield { Exemplo }
    Russian	@Yield { "\360\322\311\315\305\322" }
    Slovak	@Yield { Pr{@Char iacute}klad }
    Slovenian   @Yield { Primer }
    Spanish	@Yield { Ejemplo }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { p{@Char rcaron}ik{@Char lslash}ad }
  }
}

{ claim @Word @CurrLang @Case {
    Croatian	@Yield { Tvrdnja }
    Czech	@Yield { Tvrzen{@Char iacute} }
    Danish	@Yield { P{@Char aa}stand }
    Dutch       @Yield { Stelling }
    English	@Yield { Claim }
    EnglishUK	@Yield { Claim }
    Finnish	@Yield { V{@Char adieresis}ite }
    French	@Yield { Affirmation }
    German	@Yield { Behauptung }
    Hungarian	@Yield { kijelent{@Char eacute}s }
    Italian	@Yield { Affermazione }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Stwierdzenie }
    Portuguese  @Yield { Crédito }
    Russian	@Yield { "\365\324\327\305\322\326\304\305\316\311\305" }
    Slovak	@Yield { Tvrdenie }
    Slovenian   @Yield { Trditev }
    Spanish	@Yield { Hip{@Char oacute}tesis }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Twjerd{@Char zacute}enje }
  }
}

{ proof @Word @CurrLang @Case {
    Croatian	@Yield { Dokaz }
    Czech	@Yield { D{@Char uring}kaz }
    Danish	@Yield { Bevis }
    Dutch       @Yield { Bewijs }
    English	@Yield { Proof }
    EnglishUK	@Yield { Proof }
    Finnish	@Yield { Todistus }
    French	@Yield { Preuve }
    German	@Yield { Beweis }
    Hungarian	@Yield { Bizony{@Char iacute}t{@Char aacute}s }
    Italian	@Yield { Dimostrazione }
    Norwegian   @Yield { ?? }
    Polish	@Yield { Dow{@Char oacute}d }
    Portuguese  @Yield { Prova }
    Russian	@Yield { "\344\317\313\301\332\301\324\305\314\330\323\324\327\317" }
    Slovak	@Yield { D{@Char ocircumflex}kaz }
    Slovenian   @Yield { Dokaz }
    Spanish	@Yield { Demostraci{@Char oacute}n }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Dopokaz }
  }
}

{ abstract @Word @CurrLang @Case {
    Croatian	@Yield { Sa{@Char zcaron}etak }
    Czech	@Yield { Abstrakt }
    Danish	@Yield { Synopsis }
    Dutch       @Yield { Samenvatting }
    English	@Yield { Abstract }
    EnglishUK	@Yield { Abstract }
    Finnish	@Yield { Yleiskatsaus }
    French	@Yield { R{@Char eacute}sum{@Char eacute} }
    German	@Yield { Zusammenfassung }
    Hungarian	@Yield { Kivonat }
    Italian	@Yield { Riassunto }
    Norwegian   @Yield { Sammendrag }
    Polish	@Yield { Abstrakt }
    Portuguese  @Yield { Resumo }
    Russian	@Yield { "\341\302\323\324\322\301\313\324" } #correct!
    Slovak	@Yield { Abstrakt }
    Slovenian   @Yield { Povzetek }
    Spanish	@Yield { Sinopsis }
    Swedish	@Yield { {@Char Odieresis}versikt }
    UpperSorbian @Yield { Zje{@Char cacute}e }
  }
}

{ preface @Word @CurrLang @Case {
    Croatian	@Yield { Predgovor }
    Czech	@Yield { P{@Char rcaron}edmluva }
    Danish	@Yield { Forord }
    Dutch       @Yield { Voorwoord } # or { Woord vooraf }
    English	@Yield { Preface }
    EnglishUK	@Yield { Preface }
    Finnish	@Yield { Esipuhe }
    French	@Yield { Pr{@Char eacute}face }
    German	@Yield { Vorwort }
    Hungarian	@Yield { El{@Char odblacute}sz{@Char oacute} }
    Italian	@Yield { Prefazione }
    Norwegian   @Yield { Forord }
    Polish	@Yield { Przedmowa }
    Portuguese  @Yield { Pref{@Char aacute}cio }
    Russian	@Yield { "\360\322\305\304\311\323\314\317\327\311\305" }
    Slovak	@Yield { Predhovor }
    Slovenian   @Yield { Predgovor }
    Spanish	@Yield { Prefacio }
    Swedish	@Yield { F{@Char odieresis}rord }
    UpperSorbian @Yield { P{@Char rcaron}eds{@Char lslash}owo }
  }
}

{ abbreviations @Word @CurrLang @Case {
    Croatian	@Yield { Skra{@Char cacute}enice }
    Czech	@Yield { Zkratky }
    Danish	@Yield { ?? }
    Dutch       @Yield { ?? }
    English	@Yield { Abbreviations }
    EnglishUK	@Yield { Abbreviations }
    Finnish	@Yield { Lyhenteet }
    French	@Yield { Abr{@Char eacute}viation }
    German	@Yield { Abk{@Char udiaresis}rzungsverzeichnis }
    Hungarian	@Yield { ?? }
    Italian	@Yield { Abbreviazioni }
    Norwegian   @Yield { ?? }
    Polish	@Yield { ?? }
    Portuguese  @Yield { Abrevia{@Char ccedilla}{@Char otilde}es }
    Russian	@Yield { ?? }
    Slovak	@Yield { Skratky }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Skr{@Char oacute}t{@Char scaron}enki }
  }
}

{ introduction @Word @CurrLang @Case {
    Croatian	@Yield { Uvod }
    Czech	@Yield { {@Char Uacute}vod }
    Danish	@Yield { Indledning }
    Dutch       @Yield { Afkortingen }
    English	@Yield { Introduction }
    EnglishUK	@Yield { Introduction }
    Finnish	@Yield { Johdanto }
    French	@Yield { Introduction }
    German	@Yield { Einleitung }
    Hungarian	@Yield { Bevezet{@Char eacute}s }
    Italian	@Yield { Introduzione }
    Norwegian   @Yield { Innledning }
    Polish	@Yield { Wst{@Char eogonek}p }
    Portuguese  @Yield { Introdu{@Char ccedilla}{@Char atilde}o }
    Russian	@Yield { "\367\327\305\304\305\316\311\305" }
    Slovak	@Yield { {@Char Uacute}vod }
    Slovenian   @Yield { Uvod }
    Spanish	@Yield { Introducci{@Char oacute}n }
    Swedish	@Yield { Inledning }
    UpperSorbian @Yield { Zawod }
  }
}

{ chapter @Word @CurrLang @Case {
    Croatian	@Yield { Poglavlje }
    Czech	@Yield { Kapitola }
    Danish	@Yield { Kapitel }
    Dutch       @Yield { Hoofdstuk }
    English	@Yield { Chapter }
    EnglishUK	@Yield { Chapter }
    Finnish	@Yield { Luku }
    French	@Yield { Chapitre }
    German	@Yield { Kapitel }
    Hungarian	@Yield { fejezet }
    Italian	@Yield { Capitolo }
    Norwegian   @Yield { Kapittel }
    Polish	@Yield { Rozdzia{@Char lslash} }
    Portuguese  @Yield { Cap{@Char iacute}tolo }
    Russian	@Yield { "\347\314\301\327\301" }
    Slovak	@Yield { Kapitola }
    Slovenian   @Yield { Poglavje }
    Spanish	@Yield { Cap{@Char iacute}tulo }
    Swedish	@Yield { Kapitel }
    UpperSorbian @Yield { Staw }
  }
}

{ appendix @Word @CurrLang @Case {
    Croatian	@Yield { Dodatak }
    Czech	@Yield { P{@Char rcaron}{@Char iacute}loha }
    Danish	@Yield { Appendiks }
    Dutch       @Yield { Appendix }
    English	@Yield { Appendix }
    EnglishUK	@Yield { Appendix }
    Finnish	@Yield { Liite }
    French	@Yield { Annexe }
    German	@Yield { Anhang }
    Hungarian	@Yield { f{@Char udieresis}ggel{@Char eacute}k }
    Italian	@Yield { Appendice }
    Norwegian   @Yield { Tillegg }
    Polish	@Yield { Dodatek }
    Portuguese  @Yield { Apêndice }
    Russian	@Yield { "\360\322\311\314\317\326\305\316\311\305" }
    Slovak	@Yield { Pr{@Char iacute}loha }
    Slovenian   @Yield { Dodatek }
    Spanish	@Yield { Ap{@Char eacute}ndice }
    Swedish	@Yield { Appendix }
    UpperSorbian @Yield { Dodawki }
  }
}

{ lecture @Word @CurrLang @Case {
    Croatian	@Yield { Predavanje }
    Czech	@Yield { P{@Char rcaron}edn{@Char aacute}{@Char scaron}ka }
    Danish	@Yield { Lecture }
    Dutch       @Yield { Lecture }
    English	@Yield { Lecture }
    EnglishUK	@Yield { Lecture }
    Finnish	@Yield { Luento }
    French	@Yield { Conf{@Char eacute}rence }
    German	@Yield { Lecture }
    Hungarian	@Yield { Lecture }
    Italian	@Yield { Conferenza }
    Norwegian   @Yield { Lecture }
    Polish	@Yield { Lecture }
    Portuguese  @Yield { Conferência }
    Russian	@Yield { Lecture }
    Slovak	@Yield { Predn{@Char aacute}{@Char scaron}ka }
    Slovenian   @Yield { Lecture }
    Spanish	@Yield { Conferencia }
    Swedish	@Yield { Lecture }
    UpperSorbian @Yield { P{@Char rcaron}edno{@Char scaron}k }
  }
}

{ index @Word @CurrLang @Case {
    Croatian	@Yield { Indeks }
    Czech	@Yield { Rejst{@Char rcaron}{@Char iacute}k }
    Danish	@Yield { Indeks }
    Dutch       @Yield { Index }
    English	@Yield { Index }
    EnglishUK	@Yield { Index }
    Finnish	@Yield { Hakemisto }
    French	@Yield { Index }
    German	@Yield { Index }
    Hungarian	@Yield { Index }
    Italian	@Yield { Indice }
    Norwegian   @Yield { Register }
    Polish	@Yield { Skorowidz }
    Portuguese  @Yield { {@Char Iacute}ndice Remissivo }
    Russian	@Yield { "\351\316\304\305\313\323" }
    Slovak	@Yield { Index }
    Slovenian   @Yield { Indeks }
    Spanish	@Yield { Indice }
    Swedish	@Yield { Index }
    UpperSorbian @Yield { Indeks }
  }
}

{ notitle @Word @CurrLang @Case {
    Croatian	@Yield { Bez naslova }
    Czech	@Yield { Bez n{@Char aacute}zvu }
    Danish	@Yield { Ingen titel }
    Dutch       @Yield { Zonder titel }
    English	@Yield { No title }
    EnglishUK	@Yield { No title }
    Finnish	@Yield { Ei nime{@Char adieresis} }
    French	@Yield { Sans titre }
    German	@Yield { Ohne Titel }
    Hungarian	@Yield { C{@Char iacute}m n{@Char eacute}lk{@Char udieresis}l }
    Polish	@Yield { Bez tytu{@Char lslash}u }
    Portuguese  @Yield { Sem t{@Char iacute}tulo }
    Italian	@Yield { Senza titolo }
    Norwegian   @Yield { Utel tittel }
    Russian	@Yield { "\342\305\332 \316\301\332\327\301\316\311\321" }
    Slovak	@Yield { Bez n{@Char aacute}zvu }
    Slovenian   @Yield { Brez naslova }
    Spanish	@Yield { Sin t{@Char iacute}tulo }
    Swedish	@Yield { Ingen titel }
    UpperSorbian @Yield { Bjez titla }
  }
}

{ pages @Word @CurrLang @Case {
    Croatian	@Yield { stranica }
    Czech	@Yield { stranky }
    Danish	@Yield { sider }
    Dutch       @Yield { pagina's } # or {bladzijden}, {pag.}, {blz.}
    English	@Yield { pages }
    EnglishUK	@Yield { pages }
    Finnish	@Yield { sivut }
    French	@Yield { pages }
    German	@Yield { Seiten }
    Hungarian	@Yield { oldal }
    Italian	@Yield { pagine }
    Norwegian   @Yield { sider }
    Polish	@Yield { strony }
    Portuguese  @Yield { p{@Char aacute}ginas }
    Russian	@Yield { "\323\324\322\301\316\311\303\331" }
    Slovak	@Yield { str{@Char aacute}n }
    Slovenian   @Yield { strani }
    Spanish	@Yield { p{@Char aacute}ginas }
    Swedish	@Yield { sidor }
    UpperSorbian @Yield { strony }
  }
}

{ page @Word @CurrLang @Case {
    Croatian	@Yield { strana }
    Czech	@Yield { strana }
    Danish	@Yield { side }
    Dutch       @Yield { pagina } # or {bladzijde}, {pag.}, {blz.}
    English	@Yield { page }
    EnglishUK	@Yield { page }
    Finnish	@Yield { sivu }
    French	@Yield { page }
    German	@Yield { Seite }
    Hungarian	@Yield { oldal }
    Italian	@Yield { pagina }
    Norwegian   @Yield { side }
    Polish	@Yield { strona }
    Portuguese  @Yield { p{@Char aacute}gina }
    Russian	@Yield { "\323\324\322\301\316\311\303\301" }
    Slovak	@Yield { strana }
    Slovenian   @Yield { stran }
    Spanish	@Yield { p{@Char aacute}gina }
    Swedish	@Yield { sida }
    UpperSorbian @Yield { strona }
  }
}

{ nopublisher @Word @CurrLang @Case {
    Croatian	@Yield { Bez izdava{@Char ccaron}a }
    Czech	@Yield { Vydavatel neuveden }
    Danish	@Yield { Intet forlag }
    Dutch       @Yield { Ongepubliceerd } # or {Zonder uitgever}
    English	@Yield { No publisher }
    EnglishUK	@Yield { No publisher }
    Finnish	@Yield { Ei kustantajaa }
    French	@Yield { Sans {@Char eacute}diteur }
    German	@Yield { Ohne Verleger }
    Hungarian	@Yield { Kiad{@Char oacute} n{@Char eacute}lk{@Char udieresis}l }
    Italian	@Yield { Senza editore }
    Norwegian   @Yield { Upublisert }
    Polish	@Yield { Bez wydawcy }
    Portuguese  @Yield { Sem editora }
    Russian	@Yield { "\302\305\332 \311\332\304\301\324\305\314\321" }
    Slovak	@Yield { Vydavate{@Char lcaron} neuveden{@Char eacute} }
    Slovenian   @Yield { Brez zalo{@Char zcaron}nika }
    Spanish	@Yield { Sin editor }
    Swedish	@Yield { Inget f{@Char odieresis}rlag }
    UpperSorbian @Yield { Wudawa{@Char cacute}el njeznaty }
  }
}

{ techrep @Word @CurrLang @Case {
    Croatian	@Yield { Tehni{@Char ccaron}ko izvje{@Char scaron}{@Char cacute}e }
    Czech	@Yield { Technick{@Char aacute} zpr{@Char aacute}va }
    Danish	@Yield { Teknisk rapport }
    Dutch       @Yield { Technisch rapport }
    English	@Yield { Tech. Rep. }
    EnglishUK	@Yield { Tech. Rep. }
    Finnish	@Yield { Tekn. rap. }
    French	@Yield { Rapport technique }
    German	@Yield { Techn. Ber. }
    Hungarian	@Yield { Techn. jel. }
    Italian	@Yield { Rapporto tecnico }
    Norwegian   @Yield { Tekn. Rapp. }
    Russian	@Yield { "\324\305\310. \304\317\313\314\301\304" }
    Slovak	@Yield { Technick{@Char aacute} spr{@Char aacute}va }
    Polish	@Yield { Sprawozdanie techniczne }
    Portuguese  @Yield { Relat{@Char oacute}rio t{@Char eacute}cnico }
    Slovenian   @Yield { Tehn. por. }
    Spanish	@Yield { Informe t{@Char eacute}cnico }
    Swedish	@Yield { Tekn. rap. }
    UpperSorbian @Yield { Techn. rozp. }
  }
}

{ phdthesis @Word @CurrLang @Case {
    Croatian	@Yield { Doktorska disertacija }
    Czech	@Yield { Doktorsk{@Char aacute} disertace }
    Danish	@Yield { Ph.D.-afhandling }
    Dutch       @Yield { Proefschrift } # or {Dissertatie}
    English	@Yield { Ph.D. thesis }
    EnglishUK	@Yield { Ph.D. thesis }
    Finnish	@Yield { tohtorinv{@Char adieresis}it{@Char odieresis}skirja }
    French	@Yield { Th{@Char egrave}se de Doctorat }
    German	@Yield { Doktorarbeit }
    Hungarian	@Yield { Doktori disszert{@Char aacute}ci{@Char oacute} }
    Italian	@Yield { Tesi di dottorato }
    Norwegian   @Yield { Doktorgradsavhandling }
    Polish	@Yield { Praca doktorska }
    Portuguese  @Yield { Tese de doutoramento }
    Russian	@Yield { "\313\301\316\304\311\304\301\324\323\313\301\321"
"\304\311\323\323\305\322\324\301\303\311\321" }
    Slovak	@Yield { Doktorsk{@Char aacute} dizert{@Char aacute}cia }
    Slovenian   @Yield { Doktorska disertacija }
    Spanish	@Yield { Tesis Doctoral }
    Swedish	@Yield { doktorsavhandling }
    UpperSorbian @Yield { Disertacija }
  }
}

{ mastersthesis @Word @CurrLang @Case {
    Croatian	@Yield { Magistarski rad }
    Czech	@Yield { Diplomov{@Char aacute} pr{@Char aacute}ce }
    Danish	@Yield { Specialeafhandling }
    Dutch       @Yield { Doctoraalscriptie } 
    English	@Yield { Master's thesis }
    EnglishUK	@Yield { Master's thesis }
    Finnish	@Yield { diplomity{@Char odieresis} }
    French	@Yield { M{@Char eacute}moire de Ma{@Char icircumflex}trise }
    German	@Yield { Magisterarbeit }
    Hungarian	@Yield { Diplomamunka }
    Italian	@Yield { Tesi di laurea }
    Norwegian   @Yield { Diplomoppgave }
    Polish	@Yield { Praca magisterska }
    Portuguese  @Yield { Disserta{@Char ccedilla}{@Char atilde}o de mestrado }
    Russian	@Yield { "\304\311\320\314\317\315\316\301\321"
"\322\301\302\317\324\301" }
    Slovak	@Yield { Diplomov{@Char aacute} pr{@Char aacute}ca }
    Slovenian   @Yield { Diploma }
    Spanish	@Yield { Tesis Magistral }
    Swedish	@Yield { diplomarbete }
    UpperSorbian @Yield { Diplomowe d{@Char zacute}{@Char ecaron}{@Char lslash}o }
  }
}

{ in @Word @CurrLang @Case {
    Croatian	@Yield { U }
    Czech	@Yield { In }
    Danish	@Yield { I }
    Dutch       @Yield { In }
    English	@Yield { In }
    EnglishUK	@Yield { In }
    Finnish	@Yield { teoksessa }
    French	@Yield { dans }
    German	@Yield { In }
    Hungarian	@Yield { ?? }
    Italian	@Yield { In }
    Norwegian   @Yield { I }
    Polish	@Yield { W }
    Portuguese  @Yield { Em }
    Russian	@Yield { "\327" }
    Slovak	@Yield { In }
    Slovenian   @Yield { V }
    Spanish	@Yield { En }
    Swedish	@Yield { I }
    UpperSorbian @Yield { W }
  }
}

{ continued @Word @CurrLang @Case {
    Croatian	@Yield { nast. }
    Czech	@Yield { pokra{@Char ccaron}ov{@Char aacute}n{@Char iacute} }
    Danish	@Yield { ?? }
    Dutch       @Yield { ?? }
    English	@Yield { ctd. }
    EnglishUK	@Yield { ctd. }
    Finnish	@Yield { jatk. }
    French	@Yield { suite }
    German	@Yield { Fortsetzung }
    Hungarian	@Yield { folyt. }
    Italian	@Yield { cont. }
    Norwegian   @Yield { ?? }
    Polish	@Yield { ?? }
    Portuguese  @Yield { cont. }
    Russian	@Yield { ?? }
    Slovak	@Yield { pokra{@Char ccaron}ovanie }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { continuaci{@Char oacute}n }
    Swedish	@Yield { ?? }
    UpperSorbian @Yield { Pokro{@Char zcaron}owanje }
  }
}

{ am @Word @CurrLang @Case {
    Croatian	@Yield { a.m. }
    Czech	@Yield { a.m. }
    Danish	@Yield { a.m. }
    Dutch       @Yield { a.m. }
    English	@Yield { a.m. }
    EnglishUK	@Yield { a.m. }
    Finnish	@Yield { a.m. }
    French	@Yield { a.m. }
    German	@Yield { a.m. }
    Hungarian	@Yield { de.  }
    Italian	@Yield { a.m. }
    Norwegian   @Yield { a.m. }
    Polish	@Yield { a.m. }
    Portuguese  @Yield { a.m. }
    Russian	@Yield { a.m. }
    Slovak	@Yield { a.m. }
    Slovenian   @Yield { a.m. }
    Spanish	@Yield { a.m. }
    Swedish	@Yield { a.m. }
    UpperSorbian @Yield { dopo{@Char lslash}dnja }
  }
}

{ pm @Word @CurrLang @Case {
    Croatian	@Yield { p.m. }
    Czech	@Yield { p.m. }
    Danish	@Yield { p.m. }
    Dutch       @Yield { p.m. }
    English	@Yield { p.m. }
    EnglishUK	@Yield { p.m. }
    Finnish	@Yield { p.m. }
    French	@Yield { p.m. }
    German	@Yield { p.m. }
    Hungarian	@Yield { du.  }
    Italian	@Yield { p.m. }
    Norwegian   @Yield { p.m. }
    Polish	@Yield { p.m. }
    Portuguese  @Yield { p.m. }
    Russian	@Yield { p.m. }
    Slovak	@Yield { p.m. }
    Slovenian   @Yield { p.m. }
    Spanish	@Yield { p.m. }
    Swedish	@Yield { p.m. }
    UpperSorbian @Yield { popo{@Char lslash}dnju }
  }
}

{ shortam @Word @CurrLang @Case {
    Croatian	@Yield { am }
    Czech	@Yield { am }
    Danish	@Yield { am }
    Dutch       @Yield { am }
    English	@Yield { am }
    EnglishUK	@Yield { am }
    Finnish	@Yield { am }
    French	@Yield { am }
    German	@Yield { am }
    Hungarian	@Yield { de }
    Italian	@Yield { am }
    Norwegian   @Yield { am }
    Polish	@Yield { am }
    Portuguese  @Yield { am }
    Russian	@Yield { am }
    Slovak	@Yield { am }
    Slovenian   @Yield { am }
    Spanish	@Yield { am }
    Swedish	@Yield { am }
    UpperSorbian @Yield { dopo{@Char lslash}. }
  }
}

{ shortpm @Word @CurrLang @Case {
    Croatian	@Yield { pm }
    Czech	@Yield { pm }
    Danish	@Yield { pm }
    Dutch       @Yield { pm }
    English	@Yield { pm }
    EnglishUK	@Yield { pm }
    Finnish	@Yield { pm }
    French	@Yield { pm }
    German	@Yield { pm }
    Hungarian	@Yield { du }
    Italian	@Yield { pm }
    Norwegian   @Yield { pm }
    Polish	@Yield { pm }
    Portuguese  @Yield { pm }
    Russian	@Yield { pm }
    Slovak	@Yield { pm }
    Slovenian   @Yield { pm }
    Spanish	@Yield { pm }
    Swedish	@Yield { pm }
    UpperSorbian @Yield { popo{@Char lslash}. }
  }
}


{  ?? @Roman ??		}
{   1 @Roman i		}
{   2 @Roman ii		}
{   3 @Roman iii	}
{   4 @Roman iv		}
{   5 @Roman v		}
{   6 @Roman vi		}
{   7 @Roman vii	}
{   8 @Roman viii	}
{   9 @Roman ix		}
{  10 @Roman x		}
{  11 @Roman xi		}
{  12 @Roman xii	}
{  13 @Roman xiii	}
{  14 @Roman xiv	}
{  15 @Roman xv		}
{  16 @Roman xvi	}
{  17 @Roman xvii	}
{  18 @Roman xviii	}
{  19 @Roman xix	}
{  20 @Roman xx		}
{  21 @Roman xxi	}
{  22 @Roman xxii	}
{  23 @Roman xxiii	}
{  24 @Roman xxiv	}
{  25 @Roman xxv	}
{  26 @Roman xxvi	}
{  27 @Roman xxvii	}
{  28 @Roman xxviii	}
{  29 @Roman xxix	}
{  30 @Roman xxx	}
{  31 @Roman xxxi	}
{  32 @Roman xxxii	}
{  33 @Roman xxxiii	}
{  34 @Roman xxxiv	}
{  35 @Roman xxxv	}
{  36 @Roman xxxvi	}
{  37 @Roman xxxvii	}
{  38 @Roman xxxviii	}
{  39 @Roman xxxix	}
{  40 @Roman xl		}
{  41 @Roman xli	}
{  42 @Roman xlii	}
{  43 @Roman xliii	}
{  44 @Roman xliv	}
{  45 @Roman xlv	}
{  46 @Roman xlvi	}
{  47 @Roman xlvii	}
{  48 @Roman xlviii	}
{  49 @Roman xlix	}
{  50 @Roman l		}
{  51 @Roman li		}
{  52 @Roman lii	}
{  53 @Roman liii	}
{  54 @Roman liv	}
{  55 @Roman lv		}
{  56 @Roman lvi	}
{  57 @Roman lvii	}
{  58 @Roman lviii	}
{  59 @Roman lix	}
{  60 @Roman lx		}
{  61 @Roman lxi	}
{  62 @Roman lxii	}
{  63 @Roman lxiii	}
{  64 @Roman lxiv	}
{  65 @Roman lxv	}
{  66 @Roman lxvi	}
{  67 @Roman lxvii	}
{  68 @Roman lxviii	}
{  69 @Roman lxix	}
{  70 @Roman lxx	}
{  71 @Roman lxxi	}
{  72 @Roman lxxii	}
{  73 @Roman lxxiii	}
{  74 @Roman lxxiv	}
{  75 @Roman lxxv	}
{  76 @Roman lxxvi	}
{  77 @Roman lxxvii	}
{  78 @Roman lxxviii	}
{  79 @Roman lxxix	}
{  80 @Roman lxxx	}
{  81 @Roman lxxxi	}
{  82 @Roman lxxxii	}
{  83 @Roman lxxxiii	}
{  84 @Roman lxxxiv	}
{  85 @Roman lxxxv	}
{  86 @Roman lxxxvi	}
{  87 @Roman lxxxvii	}
{  88 @Roman lxxxviii	}
{  89 @Roman lxxxix	}
{  90 @Roman xc		}
{  91 @Roman xci	}
{  92 @Roman xcii	}
{  93 @Roman xciii	}
{  94 @Roman xciv	}
{  95 @Roman xcv	}
{  96 @Roman xcvi	}
{  97 @Roman xcvii	}
{  98 @Roman xcviii	}
{  99 @Roman xcix	}
{ 100 @Roman cc		}
{ 101 @Roman ci		}
{ 102 @Roman cii	}
{ 103 @Roman ciii	}
{ 104 @Roman civ	}
{ 105 @Roman cv		}
{ 106 @Roman cvi	}
{ 107 @Roman cvii	}
{ 108 @Roman cviii	}
{ 109 @Roman cix	}
{ 110 @Roman cx		}
{ 111 @Roman cxi	}
{ 112 @Roman cxii	}
{ 113 @Roman cxiii	}
{ 114 @Roman cxiv	}
{ 115 @Roman cxv	}
{ 116 @Roman cxvi	}
{ 117 @Roman cxvii	}
{ 118 @Roman cxviii	}
{ 119 @Roman cxix	}
{ 120 @Roman cxx	}
{ 121 @Roman cxxi	}
{ 122 @Roman cxxii	}
{ 123 @Roman cxxiii	}
{ 124 @Roman cxxiv	}
{ 125 @Roman cxxv	}
{ 126 @Roman cxxvi	}
{ 127 @Roman cxxvii	}
{ 128 @Roman cxxviii	}
{ 129 @Roman cxxix	}
{ 130 @Roman cxxx	}
{ 131 @Roman cxxxi	}
{ 132 @Roman cxxxii	}
{ 133 @Roman cxxxiii	}
{ 134 @Roman cxxxiv	}
{ 135 @Roman cxxxv	}
{ 136 @Roman cxxxvi	}
{ 137 @Roman cxxxvii	}
{ 138 @Roman cxxxviii	}
{ 139 @Roman cxxxix	}
{ 140 @Roman cxl	}
{ 141 @Roman cxli	}
{ 142 @Roman cxlii	}
{ 143 @Roman cxliii	}
{ 144 @Roman cxliv	}
{ 145 @Roman cxlv	}
{ 146 @Roman cxlvi	}
{ 147 @Roman cxlvii	}
{ 148 @Roman cxlviii	}
{ 149 @Roman cxlix	}
{ 150 @Roman cl		}
{ 151 @Roman cli	}
{ 152 @Roman clii	}
{ 153 @Roman cliii	}
{ 154 @Roman cliv	}
{ 155 @Roman clv	}
{ 156 @Roman clvi	}
{ 157 @Roman clvii	}
{ 158 @Roman clviii	}
{ 159 @Roman clix	}
{ 160 @Roman clx	}
{ 161 @Roman clxi	}
{ 162 @Roman clxii	}
{ 163 @Roman clxiii	}
{ 164 @Roman clxiv	}
{ 165 @Roman clxv	}
{ 166 @Roman clxvi	}
{ 167 @Roman clxvii	}
{ 168 @Roman clxviii	}
{ 169 @Roman clxix	}
{ 170 @Roman clxx	}
{ 171 @Roman clxxi	}
{ 172 @Roman clxxii	}
{ 173 @Roman clxxiii	}
{ 174 @Roman clxxiv	}
{ 175 @Roman clxxv	}
{ 176 @Roman clxxvi	}
{ 177 @Roman clxxvii	}
{ 178 @Roman clxxviii	}
{ 179 @Roman clxxix	}
{ 180 @Roman clxxx	}
{ 181 @Roman clxxxi	}
{ 182 @Roman clxxxii	}
{ 183 @Roman clxxxiii	}
{ 184 @Roman clxxxiv	}
{ 185 @Roman clxxxv	}
{ 186 @Roman clxxxvi	}
{ 187 @Roman clxxxvii	}
{ 188 @Roman clxxxviii	}
{ 189 @Roman clxxxix	}
{ 190 @Roman cxc	}
{ 191 @Roman cxci	}
{ 192 @Roman cxcii	}
{ 193 @Roman cxciii	}
{ 194 @Roman cxciv	}
{ 195 @Roman cxcv	}
{ 196 @Roman cxcvi	}
{ 197 @Roman cxcvii	}
{ 198 @Roman cxcviii	}
{ 199 @Roman cxcix	}
{ 200 @Roman cc		}

{  ?? @UCRoman ??	}
{   1 @UCRoman I	}
{   2 @UCRoman II	}
{   3 @UCRoman III	}
{   4 @UCRoman IV	}
{   5 @UCRoman V	}
{   6 @UCRoman VI	}
{   7 @UCRoman VII	}
{   8 @UCRoman VIII	}
{   9 @UCRoman IX	}
{  10 @UCRoman X	}
{  11 @UCRoman XI	}
{  12 @UCRoman XII	}
{  13 @UCRoman XIII	}
{  14 @UCRoman XIV	}
{  15 @UCRoman XV	}
{  16 @UCRoman XVI	}
{  17 @UCRoman XVII	}
{  18 @UCRoman XVIII	}
{  19 @UCRoman XIX	}
{  20 @UCRoman XX	}
{  21 @UCRoman XXI	}
{  22 @UCRoman XXII	}
{  23 @UCRoman XXIII	}
{  24 @UCRoman XXIV	}
{  25 @UCRoman XXV	}
{  26 @UCRoman XXVI	}
{  27 @UCRoman XXVII	}
{  28 @UCRoman XXVIII	}
{  29 @UCRoman XXIX	}
{  30 @UCRoman XXX	}
{  31 @UCRoman XXXI	}
{  32 @UCRoman XXXII	}
{  33 @UCRoman XXXIII	}
{  34 @UCRoman XXXIV	}
{  35 @UCRoman XXXV	}
{  36 @UCRoman XXXVI	}
{  37 @UCRoman XXXVII	}
{  38 @UCRoman XXXVIII	}
{  39 @UCRoman XXXIX	}
{  40 @UCRoman XL	}
{  41 @UCRoman XLI	}
{  42 @UCRoman XLII	}
{  43 @UCRoman XLIII	}
{  44 @UCRoman XLIV	}
{  45 @UCRoman XLV	}
{  46 @UCRoman XLVI	}
{  47 @UCRoman XLVII	}
{  48 @UCRoman XLVIII	}
{  49 @UCRoman XLIX	}
{  50 @UCRoman L	}
{  51 @UCRoman LI	}
{  52 @UCRoman LII	}
{  53 @UCRoman LIII	}
{  54 @UCRoman LIV	}
{  55 @UCRoman LV	}
{  56 @UCRoman LVI	}
{  57 @UCRoman LVII	}
{  58 @UCRoman LVIII	}
{  59 @UCRoman LIX	}
{  60 @UCRoman LX	}
{  61 @UCRoman LXI	}
{  62 @UCRoman LXII	}
{  63 @UCRoman LXIII	}
{  64 @UCRoman LXIV	}
{  65 @UCRoman LXV	}
{  66 @UCRoman LXVI	}
{  67 @UCRoman LXVII	}
{  68 @UCRoman LXVIII	}
{  69 @UCRoman LXIX	}
{  70 @UCRoman LXX	}
{  71 @UCRoman LXXI	}
{  72 @UCRoman LXXII	}
{  73 @UCRoman LXXIII	}
{  74 @UCRoman LXXIV	}
{  75 @UCRoman LXXV	}
{  76 @UCRoman LXXVI	}
{  77 @UCRoman LXXVII	}
{  78 @UCRoman LXXVIII	}
{  79 @UCRoman LXXIX	}
{  80 @UCRoman LXXX	}
{  81 @UCRoman LXXXI	}
{  82 @UCRoman LXXXII	}
{  83 @UCRoman LXXXIII	}
{  84 @UCRoman LXXXIV	}
{  85 @UCRoman LXXXV	}
{  86 @UCRoman LXXXVI	}
{  87 @UCRoman LXXXVII	}
{  88 @UCRoman LXXXVIII	}
{  89 @UCRoman LXXXIX	}
{  90 @UCRoman XC	}
{  91 @UCRoman XCI	}
{  92 @UCRoman XCII	}
{  93 @UCRoman XCIII	}
{  94 @UCRoman XCIV	}
{  95 @UCRoman XCV	}
{  96 @UCRoman XCVI	}
{  97 @UCRoman XCVII	}
{  98 @UCRoman XCVIII	}
{  99 @UCRoman XCIX	}
{ 100 @UCRoman CC	}
{ 101 @UCRoman CI	}
{ 102 @UCRoman CII	}
{ 103 @UCRoman CIII	}
{ 104 @UCRoman CIV	}
{ 105 @UCRoman CV	}
{ 106 @UCRoman CVI	}
{ 107 @UCRoman CVII	}
{ 108 @UCRoman CVIII	}
{ 109 @UCRoman CIX	}
{ 110 @UCRoman CX	}
{ 111 @UCRoman CXI	}
{ 112 @UCRoman CXII	}
{ 113 @UCRoman CXIII	}
{ 114 @UCRoman CXIV	}
{ 115 @UCRoman CXV	}
{ 116 @UCRoman CXVI	}
{ 117 @UCRoman CXVII	}
{ 118 @UCRoman CXVIII	}
{ 119 @UCRoman CXIX	}
{ 120 @UCRoman CXX	}
{ 121 @UCRoman CXXI	}
{ 122 @UCRoman CXXII	}
{ 123 @UCRoman CXXIII	}
{ 124 @UCRoman CXXIV	}
{ 125 @UCRoman CXXV	}
{ 126 @UCRoman CXXVI	}
{ 127 @UCRoman CXXVII	}
{ 128 @UCRoman CXXVIII	}
{ 129 @UCRoman CXXIX	}
{ 130 @UCRoman CXXX	}
{ 131 @UCRoman CXXXI	}
{ 132 @UCRoman CXXXII	}
{ 133 @UCRoman CXXXIII	}
{ 134 @UCRoman CXXXIV	}
{ 135 @UCRoman CXXXV	}
{ 136 @UCRoman CXXXVI	}
{ 137 @UCRoman CXXXVII	}
{ 138 @UCRoman CXXXVIII	}
{ 139 @UCRoman CXXXIX	}
{ 140 @UCRoman CXL	}
{ 141 @UCRoman CXLI	}
{ 142 @UCRoman CXLII	}
{ 143 @UCRoman CXLIII	}
{ 144 @UCRoman CXLIV	}
{ 145 @UCRoman CXLV	}
{ 146 @UCRoman CXLVI	}
{ 147 @UCRoman CXLVII	}
{ 148 @UCRoman CXLVIII	}
{ 149 @UCRoman CXLIX	}
{ 150 @UCRoman CL	}
{ 151 @UCRoman CLI	}
{ 152 @UCRoman CLII	}
{ 153 @UCRoman CLIII	}
{ 154 @UCRoman CLIV	}
{ 155 @UCRoman CLV	}
{ 156 @UCRoman CLVI	}
{ 157 @UCRoman CLVII	}
{ 158 @UCRoman CLVIII	}
{ 159 @UCRoman CLIX	}
{ 160 @UCRoman CLX	}
{ 161 @UCRoman CLXI	}
{ 162 @UCRoman CLXII	}
{ 163 @UCRoman CLXIII	}
{ 164 @UCRoman CLXIV	}
{ 165 @UCRoman CLXV	}
{ 166 @UCRoman CLXVI	}
{ 167 @UCRoman CLXVII	}
{ 168 @UCRoman CLXVIII	}
{ 169 @UCRoman CLXIX	}
{ 170 @UCRoman CLXX	}
{ 171 @UCRoman CLXXI	}
{ 172 @UCRoman CLXXII	}
{ 173 @UCRoman CLXXIII	}
{ 174 @UCRoman CLXXIV	}
{ 175 @UCRoman CLXXV	}
{ 176 @UCRoman CLXXVI	}
{ 177 @UCRoman CLXXVII	}
{ 178 @UCRoman CLXXVIII	}
{ 179 @UCRoman CLXXIX	}
{ 180 @UCRoman CLXXX	}
{ 181 @UCRoman CLXXXI	}
{ 182 @UCRoman CLXXXII	}
{ 183 @UCRoman CLXXXIII	}
{ 184 @UCRoman CLXXXIV	}
{ 185 @UCRoman CLXXXV	}
{ 186 @UCRoman CLXXXVI	}
{ 187 @UCRoman CLXXXVII	}
{ 188 @UCRoman CLXXXVIII}
{ 189 @UCRoman CLXXXIX	}
{ 190 @UCRoman CXC	}
{ 191 @UCRoman CXCI	}
{ 192 @UCRoman CXCII	}
{ 193 @UCRoman CXCIII	}
{ 194 @UCRoman CXCIV	}
{ 195 @UCRoman CXCV	}
{ 196 @UCRoman CXCVI	}
{ 197 @UCRoman CXCVII	}
{ 198 @UCRoman CXCVIII	}
{ 199 @UCRoman CXCIX	}
{ 200 @UCRoman CC	}


{  ?? @Alpha ??		}
{   1 @Alpha a		}
{   2 @Alpha b		}
{   3 @Alpha c		}
{   4 @Alpha d		}
{   5 @Alpha e		}
{   6 @Alpha f		}
{   7 @Alpha g		}
{   8 @Alpha h		}
{   9 @Alpha i		}
{  10 @Alpha j		}
{  11 @Alpha k		}
{  12 @Alpha l		}
{  13 @Alpha m		}
{  14 @Alpha n		}
{  15 @Alpha o		}
{  16 @Alpha p		}
{  17 @Alpha q		}
{  18 @Alpha r		}
{  19 @Alpha s		}
{  20 @Alpha t		}
{  21 @Alpha u		}
{  22 @Alpha v		}
{  23 @Alpha w		}
{  24 @Alpha x		}
{  25 @Alpha y		}
{  26 @Alpha z		}

{ 27 @Alpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char ae }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Dutch	@Yield { ?? }
    Finnish	@Yield { @Char aring }
    French	@Yield { @Char ae }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char ae }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char aring }
    UpperSorbian @Yield { ?? }
} }

{ 28 @Alpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char oslash }
    Dutch	@Yield { ?? }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Finnish	@Yield { @Char adieresis }
    French	@Yield { ?? }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char oslash }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char adieresis }
    UpperSorbian @Yield { ?? }
} }

{ 29 @Alpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char aring }
    Dutch	@Yield { ?? }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Finnish	@Yield { @Char odieresis }
    French	@Yield { ?? }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char aring }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char odieresis }
    UpperSorbian @Yield { ?? }
} }

{  ?? @UCAlpha ??	}
{   1 @UCAlpha A	}
{   2 @UCAlpha B	}
{   3 @UCAlpha C	}
{   4 @UCAlpha D	}
{   5 @UCAlpha E	}
{   6 @UCAlpha F	}
{   7 @UCAlpha G	}
{   8 @UCAlpha H	}
{   9 @UCAlpha I	}
{  10 @UCAlpha J	}
{  11 @UCAlpha K	}
{  12 @UCAlpha L	}
{  13 @UCAlpha M	}
{  14 @UCAlpha N	}
{  15 @UCAlpha O	}
{  16 @UCAlpha P	}
{  17 @UCAlpha Q	}
{  18 @UCAlpha R	}
{  19 @UCAlpha S	}
{  20 @UCAlpha T	}
{  21 @UCAlpha U	}
{  22 @UCAlpha V	}
{  23 @UCAlpha W	}
{  24 @UCAlpha X	}
{  25 @UCAlpha Y	}
{  26 @UCAlpha Z	}

{ 27 @UCAlpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char AE }
    Dutch	@Yield { ?? }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Finnish	@Yield { @Char Aring }
    French	@Yield { @Char AE }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char AE }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char Aring }
    UpperSorbian @Yield { ?? }
} }

{ 28 @UCAlpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char Oslash }
    Dutch	@Yield { ?? }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Finnish	@Yield { @Char Adieresis }
    French	@Yield { ?? }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char Oslash }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char Adieresis }
    UpperSorbian @Yield { ?? }
} }

{ 29 @UCAlpha @CurrLang @Case {
    Croatian	@Yield { ?? }
    Czech	@Yield { ?? }
    Danish	@Yield { @Char Aring }
    Dutch	@Yield { ?? }
    English	@Yield { ?? }
    EnglishUK	@Yield { ?? }
    Finnish	@Yield { @Char Odieresis }
    French	@Yield { ?? }
    German	@Yield { ?? }
    Hungarian	@Yield { ?? }
    Italian	@Yield { ?? }
    Norwegian	@Yield { @Char Aring }
    Polish	@Yield { ?? }
    Portuguese  @Yield { ?? }
    Russian	@Yield { ?? }
    Slovak	@Yield { ?? }
    Slovenian   @Yield { ?? }
    Spanish	@Yield { ?? }
    Swedish	@Yield { @Char Odieresis }
    UpperSorbian @Yield { ?? }
} }


{ 1 @Months @CurrLang @Case {
    Croatian	@Yield { sije{@Char ccaron}anj }
    Czech	@Yield { ledna }
    Danish	@Yield { januar }
    Dutch       @Yield { januari }
    English	@Yield { January }
    EnglishUK	@Yield { January }
    Finnish	@Yield { tammikuu }
    French	@Yield { janvier }
    German	@Yield { Januar }
    Hungarian	@Yield { janu{@Char aacute}r }
    Italian	@Yield { gennaio }
    Norwegian	@Yield { januar }
    Polish	@Yield { stycze{@Char nacute} }
    Portuguese  @Yield { janeiro }
    Russian	@Yield { "\321\316\327\301\322\321" }
    Slovak	@Yield { janu{@Char aacute}r }
    Slovenian   @Yield { januar }
    Spanish	@Yield { Enero }
    Swedish	@Yield { januari }
    UpperSorbian @Yield { wulkeho r{@Char oacute}{@Char zcaron}ka }
} }

{ 2 @Months @CurrLang @Case {
    Croatian	@Yield { velja{@Char ccaron}a }
    Czech	@Yield { {@Char uacute}nora }
    Danish	@Yield { februar }
    Dutch       @Yield { februari }
    English	@Yield { February }
    EnglishUK	@Yield { February }
    Finnish	@Yield { helmikuu }
    French	@Yield { f{@Char eacute}vrier }
    German	@Yield { Februar }
    Hungarian	@Yield { febru{@Char aacute}r }
    Italian	@Yield { febbraio }
    Norwegian	@Yield { februar }
    Polish	@Yield { luty }
    Portuguese  @Yield { fevereiro }
    Russian	@Yield { "\306\305\327\322\301\314\321" }
    Slovak	@Yield { febru{@Char aacute}r }
    Slovenian   @Yield { februar }
    Spanish	@Yield { Febrero }
    Swedish	@Yield { februari }
    UpperSorbian @Yield { ma{@Char lslash}eho r{@Char oacute}{@Char zcaron}ka }
} }

{ 3 @Months @CurrLang @Case {
    Croatian	@Yield { o{@Char zcaron}ujak }
    Czech	@Yield { b{@Char rcaron}ezna }
    Danish	@Yield { marts }
    Dutch       @Yield { maart }
    English	@Yield { March }
    EnglishUK	@Yield { March }
    Finnish	@Yield { maaliskuu }
    French	@Yield { mars }
    German	@Yield { M{@Char adieresis}rz }
    Hungarian	@Yield { m{@Char aacute}rcius }
    Italian	@Yield { marzo }
    Norwegian	@Yield { mars }
    Polish	@Yield { marzec }
    Portuguese  @Yield { mar{@Char ccedilla}o }
    Russian	@Yield { "\315\301\322\324\301" }
    Slovak	@Yield { marec }
    Slovenian   @Yield { marec }
    Spanish	@Yield { Marzo }
    Swedish	@Yield { mars }
    UpperSorbian @Yield { nal{@Char ecaron}tnika }
} }

{ 4 @Months @CurrLang @Case {
    Croatian	@Yield { travanj }
    Czech	@Yield { dubna }
    Danish	@Yield { april }
    Dutch       @Yield { april }
    English	@Yield { April }
    EnglishUK	@Yield { April }
    Finnish	@Yield { huhtikuu }
    French	@Yield { avril }
    German	@Yield { April }
    Hungarian	@Yield { {@Char aacute}prilis }
    Italian	@Yield { aprile }
    Norwegian	@Yield { april }
    Polish	@Yield { kwiecie{@Char nacute} }
    Portuguese  @Yield { abril }
    Russian	@Yield { "\301\320\322\305\314\321" }
    Slovak	@Yield { april }
    Slovenian   @Yield { april }
    Spanish	@Yield { Abril }
    Swedish	@Yield { april }
    UpperSorbian @Yield { jutrownika }
} }

{ 5 @Months @CurrLang @Case {
    Croatian	@Yield { svibanj }
    Czech	@Yield { kv{@Char ecaron}tna }
    Danish	@Yield { maj }
    Dutch       @Yield { mei }
    English	@Yield { May }
    EnglishUK	@Yield { May }
    Finnish	@Yield { toukokuu }
    French	@Yield { mai }
    German	@Yield { Mai }
    Hungarian	@Yield { m{@Char aacute}jus }
    Italian	@Yield { maggio }
    Norwegian	@Yield { mai }
    Polish	@Yield { maj }
    Portuguese  @Yield { maio }
    Russian	@Yield { "\315\301\321" }
    Slovak	@Yield { m{@Char aacute}j }
    Slovenian   @Yield { maj }
    Spanish	@Yield { Mayo }
    Swedish	@Yield { maj }
    UpperSorbian @Yield { ma{@Char lslash}eho r{@Char oacute}{@Char zcaron}ka }
} }

{ 6 @Months @CurrLang @Case {
    Croatian	@Yield { lipanj }
    Czech	@Yield { {@Char ccaron}ervna }
    Danish	@Yield { juni }
    Dutch       @Yield { juni }
    English	@Yield { June }
    EnglishUK	@Yield { June }
    Finnish	@Yield { kes{@Char adieresis}kuu }
    French	@Yield { juin }
    German	@Yield { Juni }
    Hungarian	@Yield { j{@Char uacute}nius }
    Italian	@Yield { giugno }
    Norwegian	@Yield { juni }
    Polish	@Yield { czerwiec }
    Portuguese  @Yield { junho }
    Russian	@Yield { "\311\300\316\321" }
    Slovak	@Yield { j{@Char uacute}n }
    Slovenian   @Yield { junij }
    Spanish	@Yield { Junio }
    Swedish	@Yield { juni }
    UpperSorbian @Yield { sma{@Char zcaron}nika }
} }


{ 7 @Months @CurrLang @Case {
    Croatian	@Yield { srpanj }
    Czech	@Yield { {@Char ccaron}ervence }
    Danish	@Yield { juli }
    Dutch       @Yield { juli }
    English	@Yield { July }
    EnglishUK	@Yield { July }
    Finnish	@Yield { hein{@Char adieresis}kuu }
    French	@Yield { juillet }
    German	@Yield { Juli }
    Hungarian	@Yield { j{@Char uacute}lius }
    Italian	@Yield { luglio }
    Norwegian	@Yield { juli }
    Polish	@Yield { lipiec }
    Portuguese  @Yield { julho }
    Russian	@Yield { "\311\300\314\321" }
    Slovak	@Yield { j{@Char uacute}l }
    Slovenian   @Yield { julij }
    Spanish	@Yield { Julio }
    Swedish	@Yield { juli }
    UpperSorbian @Yield { pra{@Char zcaron}nika }
} }

{ 8 @Months @CurrLang @Case {
    Croatian	@Yield { kolovoz }
    Czech	@Yield { srpna }
    Danish	@Yield { august }
    Dutch       @Yield { augustus }
    English	@Yield { August }
    EnglishUK	@Yield { August }
    Finnish	@Yield { elokuu }
    French	@Yield { ao{@Char ucircumflex}t }
    German	@Yield { August }
    Hungarian	@Yield { augusztus }
    Italian	@Yield { agosto }
    Norwegian	@Yield { august }
    Polish	@Yield { sierpie{@Char nacute} }
    Portuguese  @Yield { agosto }
    Russian	@Yield { "\301\327\307\325\323\324\301" }
    Slovak	@Yield { august }
    Slovenian   @Yield { avgust }
    Spanish	@Yield { Agosto }
    Swedish	@Yield { augusti }
    UpperSorbian @Yield { {@Char zcaron}njenca }
} }

{ 9 @Months @CurrLang @Case {
    Croatian	@Yield { rujan }
    Czech	@Yield { z{@Char aacute}{@Char rcaron}{@Char iacute} }
    Danish	@Yield { september }
    Dutch       @Yield { september }
    English	@Yield { September }
    EnglishUK	@Yield { September }
    Finnish	@Yield { syyskuu }
    French	@Yield { septembre }
    German	@Yield { September }
    Hungarian	@Yield { szeptember }
    Italian	@Yield { settembre }
    Norwegian	@Yield { september }
    Polish	@Yield { wrzesie{@Char nacute} }
    Portuguese  @Yield { setembro }
    Russian	@Yield { "\323\305\316\324\321\302\322\321" }
    Slovak	@Yield { september }
    Slovenian   @Yield { september }
    Spanish	@Yield { Septiembre }
    Swedish	@Yield { september }
    UpperSorbian @Yield { po{@Char zcaron}njenca }
} }

{ 10 @Months @CurrLang @Case {
    Croatian	@Yield { listopad }
    Czech	@Yield { {@Char rcaron}{@Char iacute}jna }
    Danish	@Yield { oktober }
    Dutch       @Yield { oktober }
    English	@Yield { October }
    EnglishUK	@Yield { October }
    Finnish	@Yield { lokakuu }
    French	@Yield { octobre }
    German	@Yield { Oktober }
    Hungarian	@Yield { okt{@Char oacute}ber }
    Italian	@Yield { ottobre }
    Norwegian	@Yield { oktober }
    Polish	@Yield { pa{@Char zacute}dziernik }
    Portuguese  @Yield { outubro }
    Russian	@Yield { "\317\313\324\321\302\322\321" }
    Slovak	@Yield { okt{@Char oacute}ber }
    Slovenian   @Yield { oktober }
    Spanish	@Yield { Octubre }
    Swedish	@Yield { oktober }
    UpperSorbian @Yield { winowca }
} }

{ 11 @Months @CurrLang @Case {
    Croatian	@Yield { studeni }
    Czech	@Yield { listopadu }
    Danish	@Yield { november }
    Dutch       @Yield { november }
    English	@Yield { November }
    EnglishUK	@Yield { November }
    Finnish	@Yield { marraskuu }
    French	@Yield { novembre }
    German	@Yield { November }
    Hungarian	@Yield { november }
    Italian	@Yield { novembre }
    Norwegian	@Yield { november }
    Polish	@Yield { listopad }
    Portuguese  @Yield { novembro }
    Russian	@Yield { "\316\317\321\302\322\321" }
    Slovak	@Yield { november }
    Slovenian   @Yield { november }
    Spanish	@Yield { Noviembre }
    Swedish	@Yield { november }
    UpperSorbian @Yield { nazymnika }
} }

{ 12 @Months @CurrLang @Case {
    Croatian	@Yield { prosinac }
    Czech	@Yield { prosince }
    Danish	@Yield { december }
    Dutch       @Yield { december }
    English	@Yield { December }
    EnglishUK	@Yield { December }
    Finnish	@Yield { joulukuu }
    French	@Yield { d{@Char eacute}cembre }
    German	@Yield { Dezember }
    Hungarian	@Yield { december }
    Italian	@Yield { dicembre }
    Norwegian	@Yield { desember }
    Polish	@Yield { grudzie{@Char nacute} }
    Portuguese  @Yield { dezembro }
    Russian	@Yield { "\304\305\313\301\302\322\321" }
    Slovak	@Yield { december }
    Slovenian   @Yield { december }
    Spanish	@Yield { Diciembre }
    Swedish	@Yield { december }
    UpperSorbian @Yield { hodownika }
} }


{ 1 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { sij }
    Czech	@Yield { ledna }
    Danish	@Yield { jan }
    Dutch       @Yield { jan }
    English	@Yield { Jan }
    EnglishUK	@Yield { Jan }
    Finnish	@Yield { tammikuu }
    French	@Yield { jan }
    German	@Yield { Jan }
    Hungarian	@Yield { jan }
    Italian	@Yield { gen }
    Norwegian	@Yield { jan }
    Polish	@Yield { sty }
    Portuguese  @Yield { jan }
    Russian	@Yield { "\321\316\327" }
    Slovak	@Yield { jan }
    Slovenian   @Yield { jan }
    Spanish	@Yield { Ene }
    Swedish	@Yield { jan }
    UpperSorbian @Yield { jan }
} }

{ 2 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { velj }
    Czech	@Yield { {@Char uacute}nora }
    Danish	@Yield { feb }
    Dutch       @Yield { feb }
    English	@Yield { Feb }
    EnglishUK	@Yield { Feb }
    Finnish	@Yield { helmikuu }
    French	@Yield { f{@Char eacute}v }
    German	@Yield { Feb }
    Hungarian	@Yield { feb }
    Italian	@Yield { feb }
    Norwegian	@Yield { feb }
    Polish	@Yield { luty }
    Portuguese  @Yield { fev }
    Russian	@Yield { "\306\305\327" }
    Slovak	@Yield { feb }
    Slovenian   @Yield { feb }
    Spanish	@Yield { Feb }
    Swedish	@Yield { feb }
    UpperSorbian @Yield { feb }
} }

{ 3 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { o{@Char zcaron}u }
    Czech	@Yield { b{@Char rcaron}ezna }
    Danish	@Yield { mar }
    Dutch       @Yield { mrt }
    English	@Yield { Mar }
    EnglishUK	@Yield { Mar }
    Finnish	@Yield { maaliskuu }
    French	@Yield { mar }
    German	@Yield { M{@Char adieresis}r }
    Hungarian	@Yield { m{@Char aacute}rc }
    Italian	@Yield { mar }
    Norwegian	@Yield { mar }
    Polish	@Yield { marz }
    Portuguese  @Yield { mar }
    Russian	@Yield { "\315\301\322" }
    Slovak	@Yield { mar }
    Slovenian   @Yield { mar }
    Spanish	@Yield { Mar }
    Swedish	@Yield { mar }
    UpperSorbian @Yield { m{@Char ecaron}r }
} }

{ 4 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { tra }
    Czech	@Yield { dubna }
    Danish	@Yield { apr }
    Dutch       @Yield { apr }
    English	@Yield { Apr }
    EnglishUK	@Yield { Apr }
    Finnish	@Yield { huhtikuu }
    French	@Yield { avr }
    German	@Yield { Apr }
    Hungarian	@Yield { {@Char aacute}pr }
    Italian	@Yield { apr }
    Norwegian	@Yield { apr }
    Polish	@Yield { kwie }
    Portuguese  @Yield { abr }
    Russian	@Yield { "\301\320\322" }
    Slovak	@Yield { apr }
    Slovenian   @Yield { apr }
    Spanish	@Yield { Abr }
    Swedish	@Yield { apr }
    UpperSorbian @Yield { apr }
} }

{ 5 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { svi }
    Czech	@Yield { kv{@Char ecaron}tna }
    Danish	@Yield { maj }
    Dutch       @Yield { mei }
    English	@Yield { May }
    EnglishUK	@Yield { May }
    Finnish	@Yield { toukokuu }
    French	@Yield { mai }
    German	@Yield { Mai }
    Hungarian	@Yield { m{@Char aacute}j }
    Italian	@Yield { mag }
    Norwegian	@Yield { mai }
    Polish	@Yield { maj }
    Portuguese  @Yield { mai }
    Russian	@Yield { "\315\301\321" }
    Slovak	@Yield { m{@Char aacute}j }
    Slovenian   @Yield { maj }
    Spanish	@Yield { May }
    Swedish	@Yield { maj }
    UpperSorbian @Yield { meje }
} }

{ 6 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { lip }
    Czech	@Yield { {@Char ccaron}ervna }
    Danish	@Yield { jun }
    Dutch       @Yield { jun }
    English	@Yield { Jun }
    EnglishUK	@Yield { Jun }
    Finnish	@Yield { kes{@Char adieresis}kuu }
    French	@Yield { juin }
    German	@Yield { Jun }
    Hungarian	@Yield { j{@Char uacute}n }
    Italian	@Yield { giu }
    Norwegian	@Yield { jun }
    Polish	@Yield { czer }
    Portuguese  @Yield { jun }
    Russian	@Yield { "\311\300\316\321" }
    Slovak	@Yield { j{@Char uacute}n }
    Slovenian   @Yield { jun }
    Spanish	@Yield { Jun }
    Swedish	@Yield { jun }
    UpperSorbian @Yield { jun }
} }

{ 7 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { srp }
    Czech	@Yield { {@Char ccaron}ervence }
    Danish	@Yield { jul }
    Dutch       @Yield { jul }
    English	@Yield { Jul }
    EnglishUK	@Yield { Jul }
    Finnish	@Yield { hein{@Char adieresis}kuu }
    French	@Yield { juil }
    German	@Yield { Jul }
    Hungarian	@Yield { j{@Char uacute}l }
    Italian	@Yield { lug }
    Norwegian	@Yield { jul }
    Polish	@Yield { lip }
    Portuguese  @Yield { jul }
    Russian	@Yield { "\311\300\314\321" }
    Slovak	@Yield { j{@Char uacute}l }
    Slovenian   @Yield { jul }
    Spanish	@Yield { Jul }
    Swedish	@Yield { jul }
    UpperSorbian @Yield { jul }
} }

{ 8 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { kol }
    Czech	@Yield { srpna }
    Danish	@Yield { aug }
    Dutch       @Yield { aug }
    English	@Yield { Aug }
    EnglishUK	@Yield { Aug }
    Finnish	@Yield { elokuu }
    French	@Yield { ao{@Char ucircumflex} }
    German	@Yield { Aug }
    Hungarian	@Yield { aug }
    Italian	@Yield { ago }
    Norwegian	@Yield { aug }
    Polish	@Yield { sier }
    Portuguese  @Yield { ago }
    Russian	@Yield { "\301\327\307" }
    Slovak	@Yield { aug }
    Slovenian   @Yield { avg }
    Spanish	@Yield { Ago }
    Swedish	@Yield { aug }
    UpperSorbian @Yield { awg }
} }

{ 9 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { ruj }
    Czech	@Yield { z{@Char aacute}{@Char rcaron}{@Char iacute} }
    Danish	@Yield { sep }
    Dutch       @Yield { sep }
    English	@Yield { Sep }
    EnglishUK	@Yield { Sep }
    Finnish	@Yield { syyskuu }
    French	@Yield { sep }
    German	@Yield { Sep }
    Hungarian	@Yield { szept }
    Italian	@Yield { set }
    Norwegian	@Yield { sep }
    Polish	@Yield { wrze }
    Portuguese  @Yield { set }
    Russian	@Yield { "\323\305\316" }
    Slovak	@Yield { sep }
    Slovenian   @Yield { sep }
    Spanish	@Yield { Sep }
    Swedish	@Yield { sep }
    UpperSorbian @Yield { sep }
} }

{ 10 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { lis }
    Czech	@Yield { {@Char rcaron}{@Char iacute}jna }
    Danish	@Yield { okt }
    Dutch       @Yield { okt }
    English	@Yield { Oct }
    EnglishUK	@Yield { Oct }
    Finnish	@Yield { lokakuu }
    French	@Yield { oct }
    German	@Yield { Okt }
    Hungarian	@Yield { okt }
    Italian	@Yield { ott }
    Norwegian	@Yield { okt }
    Polish	@Yield { pa{@Char zacute} }
    Portuguese  @Yield { out }
    Russian	@Yield { "\317\313\324" }
    Slovak	@Yield { okt }
    Slovenian   @Yield { okt }
    Spanish	@Yield { Oct }
    Swedish	@Yield { okt }
    UpperSorbian @Yield { okt }
} }

{ 11 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { stu }
    Czech	@Yield { listopadu }
    Danish	@Yield { nov }
    Dutch       @Yield { nov }
    English	@Yield { Nov }
    EnglishUK	@Yield { Nov }
    Finnish	@Yield { marraskuu }
    French	@Yield { nov }
    German	@Yield { Nov }
    Hungarian	@Yield { nov }
    Italian	@Yield { nov }
    Norwegian	@Yield { nov }
    Polish	@Yield { list }
    Portuguese  @Yield { nov }
    Russian	@Yield { "\316\317\321" }
    Slovak	@Yield { nov }
    Slovenian   @Yield { nov }
    Spanish	@Yield { Nov }
    Swedish	@Yield { nov }
    UpperSorbian @Yield { now }
} }

{ 12 @ShortMonths @CurrLang @Case {
    Croatian	@Yield { pro }
    Czech	@Yield { prosince }
    Danish	@Yield { dec }
    Dutch       @Yield { dec }
    English	@Yield { Dec }
    EnglishUK	@Yield { Dec }
    Finnish	@Yield { joulukuu }
    French	@Yield { d{@Char eacute}c }
    German	@Yield { Dez }
    Hungarian	@Yield { dec }
    Italian	@Yield { dic }
    Norwegian	@Yield { des }
    Polish	@Yield { gru }
    Portuguese  @Yield { dez }
    Russian	@Yield { "\304\305\313" }
    Slovak	@Yield { dec }
    Slovenian   @Yield { dec }
    Spanish	@Yield { Dic }
    Swedish	@Yield { dec }
    UpperSorbian @Yield { dec }
} }


{ 1 @WeekDays @CurrLang @Case {
    Croatian	@Yield { nedjelja }
    Czech	@Yield { ned{@Char ecaron}le }
    Danish	@Yield { s{@Char oslash}ndag }
    Dutch       @Yield { zondag }
    English	@Yield { Sunday }
    EnglishUK	@Yield { Sunday }
    Finnish	@Yield { sunnuntai }
    French	@Yield { dimanche }
    German	@Yield { Sonntag }
    Hungarian	@Yield { h{@Char eacute}tf{@Char odblacute} }
    Italian	@Yield { domenica }
    Norwegian	@Yield { s{@Char oslash}ndag }
    Polish	@Yield { niedziela }
    Portuguese  @Yield { Domingo }
    Russian	@Yield { "\327\317\323\313\322\305\323\305\316\330\305" }
    Slovak	@Yield { nede{@Char lcaron}a }
    Slovenian   @Yield { nedelja }
    Spanish	@Yield { Domingo }
    Swedish	@Yield { s{@Char odieresis}ndag }
    UpperSorbian @Yield { njed{@Char zacute}ela }
} }

{ 2 @WeekDays @CurrLang @Case {
    Croatian	@Yield { ponedjeljak }
    Czech	@Yield { pond{@Char ecaron}l{@Char iacute} }
    Danish	@Yield { mandag }
    Dutch       @Yield { maandag }
    English	@Yield { Monday }
    EnglishUK	@Yield { Monday }
    Finnish	@Yield { maanantai }
    French	@Yield { lundi }
    German	@Yield { Montag }
    Hungarian	@Yield { kedd }
    Italian	@Yield { luned{@Char igrave} }
    Norwegian	@Yield { mandag }
    Polish	@Yield { poniedzia{@Char lslash}ek }
    Portuguese  @Yield { Segunda }
    Russian	@Yield { "\320\317\316\305\304\305\314\330\316\311\313" }
    Slovak	@Yield { pondelok }
    Slovenian   @Yield { ponedeljek }
    Spanish	@Yield { Lunes }
    Swedish	@Yield { m{@Char aring}ndag }
    UpperSorbian @Yield { p{@Char oacute}nd{@Char zacute}ela }
} }

{ 3 @WeekDays @CurrLang @Case {
    Croatian	@Yield { utorak }
    Czech	@Yield { {@Char uacute}ter{@Char yacute} }
    Danish	@Yield { tirsdag }
    Dutch       @Yield { dinsdag }
    English	@Yield { Tuesday }
    EnglishUK	@Yield { Tuesday }
    Finnish	@Yield { tiistai }
    French	@Yield { mardi }
    German	@Yield { Dienstag }
    Hungarian	@Yield { szerda }
    Italian	@Yield { marted{@Char igrave} }
    Norwegian	@Yield { tirsdag }
    Polish	@Yield { wtorek }
    Portuguese  @Yield { Ter{@Char ccedilla}a }
    Russian	@Yield { "\327\324\317\322\316\311\313" }
    Slovak	@Yield { utorok }
    Slovenian   @Yield { torek }
    Spanish	@Yield { Martes }
    Swedish	@Yield { tisdag }
    UpperSorbian @Yield { wutora }
} }

{ 4 @WeekDays @CurrLang @Case {
    Croatian	@Yield { srijeda }
    Czech	@Yield { st{@Char rcaron}eda }
    Danish	@Yield { onsdag }
    Dutch       @Yield { woensdag }
    English	@Yield { Wednesday }
    EnglishUK	@Yield { Wednesday }
    Finnish	@Yield { keskiviikko }
    French	@Yield { mercredi }
    German	@Yield { Mittwoch }
    Hungarian	@Yield { cs{@Char udieresis}t{@Char odieresis}rt{@Char odieresis} }
    Italian	@Yield { mercoled{@Char igrave} }
    Norwegian	@Yield { onsdag }
    Polish	@Yield { {@Char sacute}roda }
    Portuguese  @Yield { Quarta }
    Russian	@Yield { "\323\322\305\304\301" }
    Slovak	@Yield { streda }
    Slovenian   @Yield { sreda }
    Spanish	@Yield { Mi{@Char eacute}rcoles }
    Swedish	@Yield { onsdag }
    UpperSorbian @Yield { srjeda }
} }

{ 5 @WeekDays @CurrLang @Case {
    Croatian	@Yield { {@Char ccaron}etvrtak }
    Czech	@Yield { {@Char ccaron}tvrtek }
    Danish	@Yield { torsdag }
    Dutch       @Yield { donderdag }
    English	@Yield { Thursday }
    EnglishUK	@Yield { Thursday }
    Finnish	@Yield { torstai }
    French	@Yield { jeudi }
    German	@Yield { Donnerstag }
    Hungarian	@Yield { p{@Char eacute}ntek }
    Italian	@Yield { gioved{@Char igrave} }
    Norwegian	@Yield { torsdag }
    Polish	@Yield { czwartek }
    Portuguese  @Yield { Quinta }
    Russian	@Yield { "\336\305\324\327\305\322\307" }
    Slovak	@Yield { {@Char scaron}tvrtok }
    Slovenian   @Yield { {@Char ccaron}etrtek }
    Spanish	@Yield { Jueves }
    Swedish	@Yield { torsdag }
    UpperSorbian @Yield { {@Char scaron}tw{@Char oacute}rtk }
} }

{ 6 @WeekDays @CurrLang @Case {
    Croatian	@Yield { petak }
    Czech	@Yield { p{@Char aacute}tek }
    Danish	@Yield { fredag }
    Dutch       @Yield { vrijdag }
    English	@Yield { Friday }
    EnglishUK	@Yield { Friday }
    Finnish	@Yield { perjantai }
    French	@Yield { vendredi }
    German	@Yield { Freitag }
    Hungarian	@Yield { szombat }
    Italian	@Yield { venerd{@Char igrave} }
    Norwegian	@Yield { fredag }
    Polish	@Yield { pi{@Char aogonek}tek }
    Portuguese  @Yield { Sexta }
    Russian	@Yield { "\320\321\324\316\311\303\301" }
    Slovak	@Yield { piatok }
    Slovenian   @Yield { petek }
    Spanish	@Yield { Viernes }
    Swedish	@Yield { fredag }
    UpperSorbian @Yield { pjatk }
} }

{ 7 @WeekDays @CurrLang @Case {
    Croatian	@Yield { subota }
    Czech	@Yield { sobota }
    Danish	@Yield { l{@Char oslash}rdag }
    Dutch       @Yield { zaterdag }
    English	@Yield { Saturday }
    EnglishUK	@Yield { Saturday }
    Finnish	@Yield { lauantai }
    French	@Yield { samedi }
    German	@Yield { Samstag }
    Hungarian	@Yield { vas{@Char aacute}rnap }
    Italian	@Yield { sabato }
    Norwegian	@Yield { l{@Char oslash}rdag }
    Polish	@Yield { sobota }
    Portuguese  @Yield { S{@Char aacute}bado }
    Russian	@Yield { "\323\325\302\302\317\324\301" }
    Slovak	@Yield { sobota }
    Slovenian   @Yield { sobota }
    Spanish	@Yield { S{@Char aacute}bado }
    Swedish	@Yield { l{@Char odieresis}rdag }
    UpperSorbian @Yield { sobota }
} }


{ 1 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { ned }
    Czech	@Yield { ne }
    Danish	@Yield { s{@Char oslash}n }
    Dutch       @Yield { zo }
    English	@Yield { Sun }
    EnglishUK	@Yield { Sun }
    Finnish	@Yield { su }
    French	@Yield { dim }
    German	@Yield { So }
    Hungarian	@Yield { H }
    Italian	@Yield { dom }
    Norwegian	@Yield { s{@Char oslash}n }
    Polish	@Yield { niedz }
    Portuguese  @Yield { Dom }
    Russian	@Yield { "\327\323" }
    Slovak	@Yield { ne }
    Slovenian   @Yield { ned }
    Spanish	@Yield { Dom }
    Swedish	@Yield { s{@Char odieresis} }
    UpperSorbian @Yield { nje }
} }

{ 2 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { pon }
    Czech	@Yield { po }
    Danish	@Yield { man }
    Dutch       @Yield { ma }
    English	@Yield { Mon }
    EnglishUK	@Yield { Mon }
    Finnish	@Yield { ma }
    French	@Yield { lun }
    German	@Yield { Mo }
    Hungarian	@Yield { K }
    Italian	@Yield { lun }
    Norwegian	@Yield { man }
    Polish	@Yield { pon }
    Portuguese  @Yield { Seg }
    Russian	@Yield { "\320\316" }
    Slovak	@Yield { po }
    Slovenian   @Yield { pon }
    Spanish	@Yield { Lun }
    Swedish	@Yield { m{@Char aring} }
    UpperSorbian @Yield { p{@Char oacute} }
} }

{ 3 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { uto }
    Czech	@Yield { {@Char uacute}t }
    Danish	@Yield { tir }
    Dutch       @Yield { di }
    English	@Yield { Tue }
    EnglishUK	@Yield { Tue }
    Finnish	@Yield { ti }
    French	@Yield { mar }
    German	@Yield { Di }
    Hungarian	@Yield { Sze }
    Italian	@Yield { mar }
    Norwegian	@Yield { tir }
    Polish	@Yield { wto }
    Portuguese  @Yield { Ter }
    Russian	@Yield { "\327\324" }
    Slovak	@Yield { ut }
    Slovenian   @Yield { tor }
    Spanish	@Yield { Mar }
    Swedish	@Yield { ti }
    UpperSorbian @Yield { wu }
} }

{ 4 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { sri }
    Czech	@Yield { st }
    Danish	@Yield { ons }
    Dutch       @Yield { wo }
    English	@Yield { Wed }
    EnglishUK	@Yield { Wed }
    Finnish	@Yield { ke }
    French	@Yield { mer }
    German	@Yield { Mi }
    Hungarian	@Yield { Cs }
    Italian	@Yield { mer }
    Norwegian	@Yield { ons }
    Polish	@Yield { {@Char sacute}ro }
    Portuguese  @Yield { Qua }
    Russian	@Yield { "\323\322" }
    Slovak	@Yield { st }
    Slovenian   @Yield { sre }
    Spanish	@Yield { Mie }
    Swedish	@Yield { on }
    UpperSorbian @Yield { srj }
} }

{ 5 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { {@Char ccaron}et }
    Czech	@Yield { {@Char ccaron}t }
    Danish	@Yield { tor }
    Dutch       @Yield { do }
    English	@Yield { Thu }
    EnglishUK	@Yield { Thu }
    Finnish	@Yield { to }
    French	@Yield { jeu }
    German	@Yield { Do }
    Hungarian	@Yield { P }
    Italian	@Yield { gio }
    Norwegian	@Yield { tor }
    Polish	@Yield { czw }
    Portuguese  @Yield { Qui }
    Russian	@Yield { "\336\324" }
    Slovak	@Yield { {@Char scaron}t }
    Slovenian   @Yield { cet }
    Spanish	@Yield { Jue }
    Swedish	@Yield { to }
    UpperSorbian @Yield { {@Char scaron}tw }
} }

{ 6 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { pet }
    Czech	@Yield { p{@Char aacute} }
    Danish	@Yield { fre }
    Dutch       @Yield { vr }
    English	@Yield { Fri }
    EnglishUK	@Yield { Fri }
    Finnish	@Yield { pe }
    French	@Yield { ven }
    German	@Yield { Fr }
    Hungarian	@Yield { Szo }
    Italian	@Yield { ven }
    Norwegian	@Yield { fre }
    Polish	@Yield { pi{@Char aogonek}t }
    Portuguese  @Yield { Sex }
    Russian	@Yield { "\320\324" }
    Slovak	@Yield { pi }
    Slovenian   @Yield { pet }
    Spanish	@Yield { Vie }
    Swedish	@Yield { fr }
    UpperSorbian @Yield { pj }
} }

{ 7 @ShortWeekDays @CurrLang @Case {
    Croatian	@Yield { sub }
    Czech	@Yield { so }
    Danish	@Yield { l{@Char oslash}r }
    Dutch       @Yield { za }
    English	@Yield { Sat }
    EnglishUK	@Yield { Sat }
    Finnish	@Yield { la }
    French	@Yield { sam }
    German	@Yield { Sa }
    Hungarian	@Yield { V }
    Italian	@Yield { sab }
    Norwegian	@Yield { l{@Char oslash}r }
    Polish	@Yield { sob }
    Portuguese  @Yield { S{@Char aacute}b }
    Russian	@Yield { "\323\302" }
    Slovak	@Yield { so }
    Slovenian   @Yield { sob }
    Spanish	@Yield { Sab }
    Swedish	@Yield { l{@Char odieresis} }
    UpperSorbian @Yield { so }
} }


{ 00 @TwelveHours 12 }
{ 01 @TwelveHours  1 }
{ 02 @TwelveHours  2 }
{ 03 @TwelveHours  3 }
{ 04 @TwelveHours  4 }
{ 05 @TwelveHours  5 }
{ 06 @TwelveHours  6 }
{ 07 @TwelveHours  7 }
{ 08 @TwelveHours  8 }
{ 09 @TwelveHours  9 }
{ 10 @TwelveHours 10 }
{ 11 @TwelveHours 11 }
{ 12 @TwelveHours 12 }
{ 13 @TwelveHours  1 }
{ 14 @TwelveHours  2 }
{ 15 @TwelveHours  3 }
{ 16 @TwelveHours  4 }
{ 17 @TwelveHours  5 }
{ 18 @TwelveHours  6 }
{ 19 @TwelveHours  7 }
{ 20 @TwelveHours  8 }
{ 21 @TwelveHours  9 }
{ 22 @TwelveHours 10 }
{ 23 @TwelveHours 11 }


{ 00 @ShortHours  0 }
{ 01 @ShortHours  1 }
{ 02 @ShortHours  2 }
{ 03 @ShortHours  3 }
{ 04 @ShortHours  4 }
{ 05 @ShortHours  5 }
{ 06 @ShortHours  6 }
{ 07 @ShortHours  7 }
{ 08 @ShortHours  8 }
{ 09 @ShortHours  9 }
{ 10 @ShortHours 10 }
{ 11 @ShortHours 11 }
{ 12 @ShortHours 12 }
{ 13 @ShortHours 13 }
{ 14 @ShortHours 14 }
{ 15 @ShortHours 15 }
{ 16 @ShortHours 16 }
{ 17 @ShortHours 17 }
{ 18 @ShortHours 18 }
{ 19 @ShortHours 19 }
{ 20 @ShortHours 20 }
{ 21 @ShortHours 21 }
{ 22 @ShortHours 22 }
{ 23 @ShortHours 23 }


{ 00 @MeriDiems @Word&&am }
{ 01 @MeriDiems @Word&&am }
{ 02 @MeriDiems @Word&&am }
{ 03 @MeriDiems @Word&&am }
{ 04 @MeriDiems @Word&&am }
{ 05 @MeriDiems @Word&&am }
{ 06 @MeriDiems @Word&&am }
{ 07 @MeriDiems @Word&&am }
{ 08 @MeriDiems @Word&&am }
{ 09 @MeriDiems @Word&&am }
{ 10 @MeriDiems @Word&&am }
{ 11 @MeriDiems @Word&&am }
{ 12 @MeriDiems @Word&&pm }
{ 13 @MeriDiems @Word&&pm }
{ 14 @MeriDiems @Word&&pm }
{ 15 @MeriDiems @Word&&pm }
{ 16 @MeriDiems @Word&&pm }
{ 17 @MeriDiems @Word&&pm }
{ 18 @MeriDiems @Word&&pm }
{ 19 @MeriDiems @Word&&pm }
{ 20 @MeriDiems @Word&&pm }
{ 21 @MeriDiems @Word&&pm }
{ 22 @MeriDiems @Word&&pm }
{ 23 @MeriDiems @Word&&pm }


{ 00 @ShortMeriDiems @Word&&shortam }
{ 01 @ShortMeriDiems @Word&&shortam }
{ 02 @ShortMeriDiems @Word&&shortam }
{ 03 @ShortMeriDiems @Word&&shortam }
{ 04 @ShortMeriDiems @Word&&shortam }
{ 05 @ShortMeriDiems @Word&&shortam }
{ 06 @ShortMeriDiems @Word&&shortam }
{ 07 @ShortMeriDiems @Word&&shortam }
{ 08 @ShortMeriDiems @Word&&shortam }
{ 09 @ShortMeriDiems @Word&&shortam }
{ 10 @ShortMeriDiems @Word&&shortam }
{ 11 @ShortMeriDiems @Word&&shortam }
{ 12 @ShortMeriDiems @Word&&shortpm }
{ 13 @ShortMeriDiems @Word&&shortpm }
{ 14 @ShortMeriDiems @Word&&shortpm }
{ 15 @ShortMeriDiems @Word&&shortpm }
{ 16 @ShortMeriDiems @Word&&shortpm }
{ 17 @ShortMeriDiems @Word&&shortpm }
{ 18 @ShortMeriDiems @Word&&shortpm }
{ 19 @ShortMeriDiems @Word&&shortpm }
{ 20 @ShortMeriDiems @Word&&shortpm }
{ 21 @ShortMeriDiems @Word&&shortpm }
{ 22 @ShortMeriDiems @Word&&shortpm }
{ 23 @ShortMeriDiems @Word&&shortpm }


{ date @DateTimeFormat @Value { @CurrLang @Case {
    Croatian	@Yield { @DayNum. @Month @Year                  }
    Czech	@Yield { @DayNum. @Month @Year			}
    Danish	@Yield { @DayNum. @Month, @Year			}
    Dutch	@Yield { @DayNum @Month @Year			}
    English	@Yield { @DayNum @Month, @Year			}
    EnglishUK	@Yield { @DayNum @Month, @Year			}
    Finnish	@Yield { @DayNum. @Month"ta" @Year		}
    French	@Yield { @DayNum @Month @Year			}
    German	@Yield { @DayNum. @Month @Year			}
    Hungarian	@Yield { @Year. @Month @DayNum.			}
    Italian	@Yield { @DayNum @Month @Year			}
    Norwegian   @Yield { @DayNum @Month, @Year			}
    Polish	@Yield { @DayNum @Month @Year			}
    Portuguese  @Yield { @DayNum "de" @Month "de" @Year		}
    Russian	@Yield { @DayNum @Month @Year			}
    Slovak	@Yield { @DayNum. @Month @Year			}
    Slovenian   @Yield { @DayNum @Month @Year			}
    Spanish	@Yield { @DayNum "de" @Month, @Year		}
    Swedish	@Yield { @DayNum @Month @Year			}
    UpperSorbian @Yield { @DayNum @Month @Year			}
  } }
}

{ time @DateTimeFormat @Value { @CurrLang @Case {
    Croatian	@Yield { @ShortHour:@Minute                     }
    Czech	@Yield { @ShortHour:@Minute			}
    Danish	@Yield { @ShortHour.@Minute			}
    Dutch	@Yield { @ShortHour.@Minute			}
    English	@Yield { @TwelveHour.@Minute @MeriDiem		}
    EnglishUK	@Yield { @TwelveHour.@Minute @MeriDiem		}
    Finnish	@Yield { @ShortHour.@Minute			}
    French	@Yield { @ShortHour"h"@Minute			}
    German	@Yield { @ShortHour.@Minute			}
    Hungarian	@Yield { @ShortHour {@Char oacute}ra @Minute perc }
    Italian	@Yield { @ShortHour:@Minute			}
    Norwegian   @Yield { @ShortHour.@Minute 			}
    Polish	@Yield { @ShortHour.@Minute			}
    Portuguese  @Yield { @ShortHour:@Minute			}
    Russian	@Yield { @ShortHour.@Minute			}
    Slovak	@Yield { @ShortHour:@Minute			}
    Slovenian   @Yield { @ShortHour.@Minute			}
    Spanish	@Yield { @ShortHour:@Minute 			}
    Swedish	@Yield { @ShortHour.@Minute			}
    UpperSorbian @Yield { @ShortHour.@Minute			}
  } }
}
