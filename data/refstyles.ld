{ Book @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  {Slope @Font @Title}.  @Publisher, @Year.  @Comment }
  }
}

{ JournalArticle @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  @Title.  {Slope @Font @Journal} {Bold @Font @Volume},
@Pages (@Year).  @Comment
     }
  }
}

{ ConferencePaper @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  @Title.  {Slope @Font @Proceedings},
@Year, @Pages.  @Comment
     }
  }
}

{ TechReport @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  {Slope @Font @Title}.  Tech. Rep. @Number (@Year),
@Institution.  @Comment
     }
  }
}

{ PhD @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  {Slope @Font @Title}.  Ph.D. thesis,
@Institution (@Year).  @Comment
     }
  }
}

{ Software @RefStyle @Style
  {  @Reference&&reftag @Open
     { @Author,  {Slope @Font @Title}, @Year.  @Comment
     }
  }
}
