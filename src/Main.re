/*
 exports.handler = function(event, context, callback) {
     console.log('Event: ', JSON.stringify(event, null, '\t'));
     console.log('Context: ', JSON.stringify(context, null, '\t'));
     callback(null);
 };
 */
let handler = (_event, _context, callback) => {
  let msg = "Hello, mum!!";
  Js.log(msg);
  callback(. Js.null, msg);
};
