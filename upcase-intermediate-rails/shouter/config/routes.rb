Shouter::Application.routes.draw do
  root to: 'homes#show', via: :get

  # There is only one dashboard, so we use resource.
  # /dashboard/, not /dashboards/:id

  resource :dashboard, only: [:show]
  resource :session, only: [:new, :create, :destroy]
  resources :users, only: [:new, :create]
end
