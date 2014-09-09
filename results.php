<?php
try {
  $dbh = new PDO("sqlite:investors.sqlite3");
} catch(PDOException $e) {
    echo $e->getMessage();
    return;
}
$prepared_query = $dbh->prepare(
  'SELECT * FROM mailings WHERE partner_code = ?'
);
$prepared_query->execute([$_GET['partner_code']]);
$data = $prepared_query->fetchAll();

/*
 * We've got the data. Time to build the response.
 */

/* If they asked for JSON */
if ($_GET['json']) {
  $json = json_encode($data);

  /*
   * JSONP callback wrapper if a callback is specified
   */
  if (isset($_GET['callback'])) {
    $json = $_GET['callback'] . '(' . $json . ');';
    $header = 'Content-type: application/javascript';
  } else {
    $header = 'Content-type: application/json';
  }

  header($header);
  echo $json;
  return;
}

?>
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width">
  <title>Perivan Solutions Investor Order Lookup</title>
</head>
<body>

<?php
/*
 * If nothing has been found
 */
if (count($data) == 0) {
?>
<p>
  Record not found. Please ensure that your partner code is correct.
</p>
<p>
  <a href='index.html'>Click here to return</a>
</p>
<?php
  return;
}

/*
 * If we've found something
 */
/* Loop over the table here */
?>

TODO: Table goes here

<?php

var_dump($data);

?>
</body>
</html>
