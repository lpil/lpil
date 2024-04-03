#!/usr/bin/env node

// YYYY-MM-DD hh:mm
var timeNowStr = function timeNowStr() {
  var now   = new Date(),
      year  = now.getFullYear(),
      month = now.getMonth() + 1,
      day   = now.getDate(),
      hour  = now.getHours(),
      mins  = now.getMinutes();

  var leadingZero = function leadingZero(num) {
    if (num < 10) {
      return '0' + num;
    } else {
      return num;
    }
  };

  month = leadingZero(month);
  day   = leadingZero(day);
  hour  = leadingZero(hour);
  mins  = leadingZero(mins);

  return year + '-' +
         month + '-' +
         day + ' ' +
         hour + ':' +
         mins + '\n';
};

require('net')
  .createServer(function(socket) {
    socket.end(timeNowStr());
  })
  .listen(process.argv[2]);
