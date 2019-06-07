module Schema = Graphql_schema.Make (struct
  type +'a t = 'a Js.Promise.t

  let bind x f = Js.Promise.then_ f x

  let return = Js.Promise.resolve
end)
