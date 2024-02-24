Rails.application.routes.draw do
  get '/collections/:id/users', to: 'collections#users'
  get '/collections/archive', to: 'collections#archive'
  patch '/collections/:id/restore', to: 'collections#restore'
  resources :collections

  get '/users/current', to: 'users#current'
  resources :users

  get '/categories/list', to: 'categories#list'
  get '/categories/archive', to: 'categories#archive'
  patch '/categories/:id/restore', to: 'categories#restore'
  resources :categories

  # Authentication
  resources :sessions, only: [:new, :create, :destroy]
  get '/signin',  to: 'sessions#new'
  delete '/signout', to: 'sessions#destroy'

  root 'sessions#new', via: :get
end
