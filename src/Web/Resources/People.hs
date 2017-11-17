module Web.Resources.People where

import qualified Web.Actions.People as PeopleA
import Web.Configuration.Response (Api)
import Web.Spock ((<//>), delete, get, post, put, var)

resources :: Api
resources = do
    get "people" PeopleA.index
    get ("people" <//> var) PeopleA.show
    put ("people" <//> var) PeopleA.update
    delete ("people" <//> var) PeopleA.destroy
    post "people" PeopleA.create
