let component = ReasonReact.statelessComponent("Greeting");

let make = (~message, _children) => {
  ...component,
  render: _self => <div> (ReasonReact.stringToElement(message)) </div>,
};
