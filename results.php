<?php
try {
  $dbh = new PDO("sqlite:investors.sqlite3");
} catch(PDOException $e) {
    echo $e->getMessage();
    return;
}
$prepared_query = $dbh->prepare(
  'SELECT * FROM investors WHERE partner_code = ?'
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
  <style media="screen" type="text/css">
* {
  font-family: Verdana, Helvetica, sans-serif;
  font-size: 80%;
}
td:first-child {
  padding-right: 15px;
}
  </style>
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
?>
<h3>
  Investor & Investment CD order summary
</h3>
<p>
  This summary shows the order placed for the last edition. The details will
  not reflect any changes you may have made for the next edition.
</p>
<?php
/* Loop over the table here */
foreach ($data as $index=>$order) {
?>
<h2>
  Address <?php echo $index + 1; ?> of <?php echo count($data); ?>
</h2>
<table>
  <tr>
    <td>Name</td>
    <td><?php echo $order['name']; ?></td>
  </tr>
  <tr>
    <td>Partner Code</td>
    <td><?php echo $order['partner_code']; ?></td>
  </tr>
  <tr><td><br></td></tr>
  <tr>
    <td>Investor Delivery Address</td>
    <td><?php echo $order['address0']; ?></td>
  </tr>
  <tr>
    <td><i>Please note for Personalised Only</i></td>
    <td><?php echo $order['address1']; ?></td>
  </tr>
  <tr>
    <td><i>this may differ to your inside front cover</i></td>
    <td><?php echo $order['address2']; ?></td>
  </tr>
  <tr>
    <td></td>
    <td><?php echo $order['address3']; ?></td>
  </tr>
  <tr>
    <td></td>
    <td><?php echo $order['post_code']; ?></td>
  </tr>
  <tr><td><br></td></tr>
  <tr>
    <td>Personalised Investor</td>
    <td><?php echo $order['pim_quantity']; ?></td>
  </tr>
  <tr>
    <td>Investment Review</td>
    <td><?php echo $order['qir_quantity']; ?></td>
  </tr>
  <tr>
    <td>Generic Investor</td>
    <td><?php echo $order['gim_quantity']; ?></td>
  </tr>
  <tr><td><br></td></tr>
  <tr>
    <td><b>CDs - Spring & Autumn Editions ONLY</b></td>
  </tr>
  <tr>
    <td>Investment CD</td>
  </tr>
  <tr>
    <td>&bull; Generic CD &amp; Wallet</td>
    <td><?php echo $order['cd_wallet']; ?></td>
  </tr>
  <tr>
    <td>&bull; Generic CD &amp; Personalised Wallet</td>
    <td><?php echo $order['cd_custom_wallet']; ?></td>
  </tr>
  <tr>
    <td>&bull; Personalised Wallet &amp; Recording</td>
    <td><?php echo $order['cd_custom_wallet_rec']; ?></td>
  </tr>
  <tr><td><br></td></tr>
  <tr>
    <td>Trustee CD</td>
    <td></td>
  </tr>
  <tr>
    <td>&bull; Generic</td>
    <td><?php echo $order['trust_cd_generic']; ?></td>
  </tr>
  <tr>
    <td>&bull; Personalised</td>
    <td><?php echo $order['trust_cd_personal']; ?></td>
  </tr>
</table>
<p>
  If you do not make any changes this order will continue for future editions.
</p>
<?php } ?>
</body>
</html>
