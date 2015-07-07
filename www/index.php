
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> This is the homepage of the project <b>Chain Graph Models in R</b>. On this page you can find links to software, papers, talks, data related to the package <b>gRchain</b>. Below you can also find will a tutorial on using the functions from <b>gRchain</b>.
</p>

<!--<h3>Papers:</h3>
<p><a href="http://epub.wu.ac.at/4476/">Technical Report on Cluster Optimized Proximity Scaling (COPS)</a> </p>
--->

<h3>Talks:</h3>
<table>
<tr>
  <th>Talk</th>
  <th>Date</th>
  <th>Place</th>
</tr> 
<tr>
  <td>Psychoco 2013 (<a href="http://epub.wu.ac.at/3781/">slides</a>)</td>
  <td>14.02.2013-15.02.2013</td>
  <td>Zuerich, Switzerland</td>
</tr> 
</table>

<h3>Software:</h3>
<p> The <strong>project summary page and the software</strong> you can usually find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p> 

<p>Until the current issues with R-Forge are fixed however, you can get package binaries here too:</p> 
<dl>
<li><a href="gRchain_current.tar.gz">gRchain Package Source</>
<li><a href="gRchain_current.zip">gRchain for Windows</>  
</dl>

<h3>People:</h3>
<dl>
<li><a href="http://www.wu.ac.at/methods/team/dr-thomas-rusch/en/">Thomas Rusch</a></li> 
<li><a href="http://www.wu.ac.at/statmath/faculty-staff/projects/mwurzer/">Marcus Wurzer</a></li>
<li><a href="http://www.wu.ac.at/sm/team/faculty/gruber/">Kathrin Gruber</a></li>
</dl>

</body>
</html>

<?php 
readfile("gRchain.html");
?>
