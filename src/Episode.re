type t = {
  name: string,
  number: int
};

/*
   Mock data
 */
let hope = {name: "A New Hope", number: 4};

let empire = {name: "The Empire Strikes Back", number: 5};

let jedi = {name: "The Return of the Jedi", number: 6};

/*
   Persistence functions
 */
let all () => [|hope, empire, jedi|];

let favourite () => empire;

let count () => Array.length (all ());
