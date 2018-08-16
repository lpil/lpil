# https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters

case Mix.env() do
  :dev ->
    import Boilerplate.Repo, only: [insert!: 1]
    alias Boilerplate.{User, Organisation, Membership}

    #
    # Organisations
    #

    ravenclaw =
      insert!(%Organisation{
        name: "Ravenclaw"
      })

    hufflepuff =
      insert!(%Organisation{
        name: "Hufflepuff"
      })

    slytherin =
      insert!(%Organisation{
        name: "Slytherin"
      })

    gryffindor =
      insert!(%Organisation{
        name: "Gryffindor"
      })

    teachers =
      insert!(%Organisation{
        name: "Hogwarts Teachers"
      })

    ministry =
      insert!(%Organisation{
        name: "Ministry of Magic"
      })

    dumbledores_army =
      insert!(%Organisation{
        name: "Dumbledore's Army"
      })

    umbridges_squad =
      insert!(%Organisation{
        name: "Umbridge's Inquisitorial Squad"
      })

    #
    # Users
    #

    luna =
      insert!(%User{
        name: "Lune Lovegood",
        email: "luna.lovegood@thequibbler.net"
      })

    dean =
      insert!(%User{
        name: "Dean Thmoas",
        email: "dean.thomas@footballfanz.co.uk"
      })

    cho =
      insert!(%User{
        name: "Cho Chang",
        email: "cho.chang@dumbledoresarmy.org"
      })

    hannah =
      insert!(%User{
        name: "Hannah Abbott",
        email: "hannah.abbott@dumbledoresarmy.org"
      })

    millicent =
      insert!(%User{
        name: "Millicent Bulstrode",
        email: "millicent.bulstrode@inquisitorial-squad.gov"
      })

    mcgonagall =
      insert!(%User{
        name: "Minerva McGonagall",
        email: "minerva.mcgonagall@hogwarts.edu"
      })

    flitwick =
      insert!(%User{
        name: "Filius Flitwick",
        email: "filius.flitwick@hogwarts.edu"
      })

    sprout =
      insert!(%User{
        name: "Pomona Sprout",
        email: "pomona.sprout@hogwarts.edu"
      })

    snape =
      insert!(%User{
        name: "Severus Snape",
        email: "severus.snape@hogwarts.edu"
      })

    fudge =
      insert!(%User{
        name: "Cornelius Fudge",
        email: "cornelius.fudge@ministryofmagic.gov"
      })

    scrimgeour =
      insert!(%User{
        name: "Rufus Scrimgeour",
        email: "rufus.scrimgeour@ministryofmagic.gov"
      })

    #
    # Memberships
    #

    [
      {luna, gryffindor},
      {luna, dumbledores_army},
      {dean, gryffindor},
      {dean, dumbledores_army},
      {cho, ravenclaw},
      {cho, dumbledores_army},
      {hannah, hufflepuff},
      {hannah, dumbledores_army},
      {millicent, slytherin},
      {millicent, umbridges_squad},
      {mcgonagall, teachers},
      {mcgonagall, gryffindor, :admin},
      {flitwick, teachers},
      {flitwick, ravenclaw, :admin},
      {sprout, teachers},
      {sprout, hufflepuff, :admin},
      {snape, teachers},
      {snape, slytherin, :admin},
      {fudge, ministry, :admin},
      {scrimgeour, ministry, :admin}
    ]
    |> Enum.map(fn
      {user, organisation} ->
        insert!(%Membership{type: :user, user_id: user.id, organisation_id: organisation.id})

      {user, organisation, type} ->
        insert!(%Membership{type: type, user_id: user.id, organisation_id: organisation.id})
    end)

  _ ->
    :ok
end
