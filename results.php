<?php
$dpd_url = 'http://www.dpd.co.uk/apps/tracking/?reference=';

try {
  $dbh = new PDO("sqlite:orders.sqlite3");
} catch(PDOException $e) {
    echo $e->getMessage();
    return;
}
$prepared_query = $dbh->prepare(
  'SELECT * FROM mailings WHERE order_ref = ?'
);

$prepared_query->execute([$_GET['order_ref']]);
$data = $prepared_query->fetchAll()[0];
if ($data) {
  $data['url'] = $dpd_url . $data['dpd_ref'];
}

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
  <title>Perivan Solutions Order Tracking</title>
</head>
<body>

<?php
/*
 * If nothing has been found
 */
if (!$data) {
?>
<p>
  No package found with that reference.
</p>
<p>
  Either your reference is incorrect, or your package has not yet been sent.
</p>
<?php
  return;
}

/*
 * If it has been sent by post
 */
if ($data['is_post'] == 't') {
?>
  <p>
    Your package was sent by post on <?php echo $data['date_sent']; ?>
  </p>
<?php

/*
 * If it has been sent by DPD
 */
} else {
?>
  <p>
    Your package was sent by DPD on <?php echo $data['date_sent']; ?>
  </p>
  <p>
    Your DPD tracking reference is: <a href="<?php echo $data['url'] ?>"
    target="_blank"><?php echo $data['dpd_ref'] ?></a>
  </p>
<?php
}
?>
</body>
</html>
