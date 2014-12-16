Shouter::Application.routes.draw do
  root to: 'homes#show', via: :get

  # There is only one dashboard, so we use resource.
  # /dashboard/, not /dashboards/:id

  resource :dashboard, only: [:show]
  resource :session, only: [:new, :create, :destroy]

  resources :shouts, only: [:create, :show]
  resources :users, only: [:new, :show, :create]
end
