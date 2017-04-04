package com.github.johnynek.paiges

private[paiges] object Strings {
  private[this] val maxSpaceTable = 100

  private[this] val spaceArray: Array[String] =
    (1 to maxSpaceTable).map { i => " " * i }.toArray

  def spaceString(n: Int): String =
    if (n == 0) ""
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else (" " * n)


  private def makeIndentStr(i: Int): String = "\n" + (" " * i)
  private[this] val indentTable: Array[String] =
    (0 until maxSpaceTable).iterator
      .map(makeIndentStr)
      .toArray

  def indentString(indent: Int): String =
    if (indent < maxSpaceTable) indentTable(indent)
    else makeIndentStr(indent)
}
