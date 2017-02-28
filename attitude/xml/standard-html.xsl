<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0"
  >
  <xsl:output method="html"/>




<!-- wrap the whole thing in html -->
  <xsl:template match="/">
    <html>
    <head>

    <title><xsl:value-of select="/HELPFILE/TOOL"/> HEADAS help file</title>

    </head>
    <body>
       <xsl:apply-templates/>
    </body>
    </html>
  </xsl:template>
  


<!-- top level header -->
  <xsl:template match="/HELPFILE">
    <h2>NAME</h2>
    <xsl:apply-templates select="TOOL"/> - <xsl:apply-templates select="BRIEFLY"/>

    <!-- construct the usage section from information found in the document -->
    <h2>USAGE</h2>
    <tt>
    <xsl:value-of select="TOOL"/>

    <!-- loop over parameters -->
    <xsl:for-each select="PARAMETERS/PARAM">
        <xsl:if test="not(MODE='h')">
            <xsl:value-of select="' '"/>
            <xsl:value-of select="NAME"/>=&lt;<xsl:apply-templates select="FORMAT"/>&gt;
        </xsl:if>
    </xsl:for-each>
    </tt>

    <!-- now do everything else -->
    <xsl:apply-templates select="/HELPFILE/DESCRIPTION"/>
    <xsl:apply-templates select="/HELPFILE/PARAMETERS"/>
    <xsl:apply-templates select="/HELPFILE/EXAMPLES"/>
    <xsl:apply-templates select="/HELPFILE/SEEALSO"/>
    <xsl:apply-templates select="/HELPFILE/DATE"/>
  </xsl:template>

<!-- major sections -->
  <xsl:template match="/HELPFILE/DESCRIPTION">
    <h2>DESCRIPTION</h2>
    <xsl:apply-templates/>
  </xsl:template>



  <xsl:template match="/HELPFILE/EXAMPLES">
      <h2>EXAMPLES</h2>
      <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="/HELPFILE/SEEALSO">
      <h2>SEE ALSO</h2>
      <xsl:apply-templates/>
  </xsl:template>
  

  <xsl:template match="/HELPFILE/DATE">
      <h2>LAST MODIFIED</h2>
      <xsl:apply-templates select="MONTH"/>
      <xsl:value-of select="' '"/>
      <xsl:apply-templates select="YEAR"/> 

  </xsl:template>



<!-- wrap text in a paragraph if it isn't already in one -->
    <xsl:template match="ITEM/text()|PARAM/text()">

        <xsl:if test="normalize-space()">

            <p><xsl:value-of select="normalize-space()"/></p>

        </xsl:if>
    </xsl:template>
    
<!-- hyperlinks -->
    <xsl:template match="LINK">
        <xsl:element name="a">
            <xsl:attribute name="href">
                <xsl:value-of select="concat(normalize-space(),'.html')"/>
            </xsl:attribute>
            <xsl:value-of select="normalize-space()"/>
        </xsl:element>
    </xsl:template>


    <!-- list parameters with hidden ones last and sorted alphabetically -->

    <xsl:template match="/HELPFILE/PARAMETERS">
        <h2>PARAMETERS</h2>
        <dl>
        <xsl:for-each select="PARAM">
            <xsl:sort select="MODE"/>
            <xsl:sort select="NAME"/>
            <xsl:apply-templates select="."/>
        </xsl:for-each>
        </dl>
    </xsl:template>

<!-- parameter -->
  <xsl:template match="/HELPFILE/PARAMETERS/PARAM">
  <!-- heading -->
  <xsl:if test="MODE='h'">
      <!-- hidden parameter -->
      <dt> (<xsl:apply-templates select="NAME"/> = <xsl:apply-templates select="VALUE"/>) [<xsl:apply-templates select="FORMAT"/>]</dt>
  </xsl:if>
  
  <xsl:if test="not(MODE='h')">
      <!-- non-hidden parameter -->
       <dt> <xsl:apply-templates select="NAME"/> [<xsl:apply-templates select="FORMAT"/>]</dt>
   </xsl:if>
   <dd>
       <xsl:apply-templates select="text()|P|LIST"/>
   </dd>

  </xsl:template>

  <!-- translate format codes into words -->
  <xsl:template match="PARAM/FORMAT">
      <xsl:if test="normalize-space()='i'">integer</xsl:if>
      <xsl:if test="normalize-space()='b'">boolean</xsl:if>
      <xsl:if test="normalize-space()='r'">real</xsl:if>
      <xsl:if test="normalize-space()='f'">file</xsl:if>
      <xsl:if test="normalize-space()='s'">string</xsl:if>
  </xsl:template>

  <!-- date formatting -->
    <xsl:template match="DATE/MONTH">
        <xsl:if test="normalize-space()='1'">January</xsl:if>
        <xsl:if test="normalize-space()='2'">February</xsl:if>
        <xsl:if test="normalize-space()='3'">March</xsl:if>
        <xsl:if test="normalize-space()='4'">April</xsl:if>
        <xsl:if test="normalize-space()='5'">May</xsl:if>
        <xsl:if test="normalize-space()='6'">June</xsl:if>
        <xsl:if test="normalize-space()='7'">July</xsl:if>
        <xsl:if test="normalize-space()='8'">August</xsl:if>
        <xsl:if test="normalize-space()='9'">September</xsl:if>
        <xsl:if test="normalize-space()='10'">October</xsl:if>
        <xsl:if test="normalize-space()='11'">November</xsl:if>
        <xsl:if test="normalize-space()='12'">December</xsl:if>
    </xsl:template>
    



  <!-- general text formatting stuff -->

  <xsl:template match="P">
    <P>
    <xsl:apply-templates/>
    </P>
  </xsl:template>


  <xsl:template match="LIST">
    <xsl:if test="@type='ordered'">
      <ol>
        <xsl:apply-templates/>
      </ol>
    </xsl:if>
    <xsl:if test="@type='unordered'">
      <ul>
        <xsl:apply-templates/>
      </ul>
    </xsl:if>
  </xsl:template>
  

  <xsl:template match="ITEM">
    <li><xsl:apply-templates/>
    </li>
  </xsl:template>

</xsl:stylesheet>
