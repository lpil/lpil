class CollectionsController < ApplicationController
  # Only admins can manipulate Collections
  before_filter do
    block_access unless current_user && current_user.admin?
  end

  def index
    @collections = Collection.all
  end

  def new
    @collection = Collection.new
  end

  def show
    @collection = Collection.find params[:id]
  end

  def edit
    @collection = Collection.find params[:id]
  end

  def create
    @collection = Collection.new params[:collection].permit :name
    if @collection.save
      flash[:success] = 'New collection successfully created'
      redirect_to @collection
    else
      render :new
    end
  end

  def destory
    collection = Collection.find params[:id]
    collection.locked = true
    collection.save!
    flash[:success] = 'Collection successfully archived'
    redirect_to '/collections'
  end

  def users
    @collection = Collection.includes(:users).find_by params[:id]
    @users = @collection.users if @collection
  end
end
