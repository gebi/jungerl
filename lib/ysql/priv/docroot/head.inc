<html>
<head>
<title>Yaws SQL Browser</title>
 
<link rel="STYLESHEET" type="text/css" href="ysql.css">
<script type="text/javascript" src="prototype.yaws"></script>
<script type="text/javascript" src="fd2qs.yaws"></script>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
</head>
<body class="body"> 
<table border="0" cellpadding="0" cellspacing="0" width="99%">
  <tbody>
   <tr>
   <td class=logo>Yaws SQL Browser</td>
   <td ><span class="current_database_txt">(current database is:</span> <span class="current_database">%%DB%%</span><span class="current_database_txt">)</span></td>
   <td align="right" valign="bottom" class="date"><div id="the_clock"></div></td>
   </tr>
   <tr>
   </tr>
   <tr>
   <td colspan="3" bgcolor="#919af5"><img src="blank.html" height="2"></td>
   </tr>
  </tbody>
</table>
<table border="0" cellpadding="2" cellspacing="0" width="99%">
  <tbody>
   <tr>
   <td class="headerLinks" align="left">
    <a href="/%%DTABLE_LNK%%" class="headerLinks" title="Describe table">%%DTABLE%%</a>
   </td>
   <td class="headerLinks" align="right">
    <a href="/top.yaws" class="headerLinks" title="Show all Databases">Databases</a> |
    <a href="/%%TABLES%%" title="Show all tables" class="headerLinks">Tables</a> |
    <a href="/sql_query.yaws" class="headerLinks" title="Submit an SQL query">Query</a> |
    <a href="/logout.yaws" class="headerLinks" title="Logout from YSQL">Logout</a>
   </td>
  </tr>
 </tbody>
</table>
<script>
%%CLOCK%%
</script>