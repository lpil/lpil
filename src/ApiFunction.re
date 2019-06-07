let handler = [%bs.raw
  {| (event, context, callback) => {
  const response = {
    statusCode: 200,
    headers: {
        'Content-Type': 'text/html; charset=utf-8',
    },
    body: "<p>Hello Sailor</p>",
  };
  callback(null, response);
}

|}
];
