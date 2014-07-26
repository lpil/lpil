class CollectionsController < ApplicationController
  # Only admins can manipulate Collections
  before_filter do
    block_access unless current_user && current_user.admin?
  end

  def index
    @collections = Collection.where locked: false
  end

  def archive
    @collections = Collection.where locked: true
  end

  def new
    @collection = Collection.new
  end

  def show
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

  def edit
    @collection = Collection.find params[:id]
  end

  def update
    require 'pry'; binding.pry
    @collection = Collection.find params[:id]
    if @collection.update_attributes(
      params[:collection].permit :name)

      flash[:success] = "#{@collection.name} successfully updated"
      redirect_to @collection
    else
      render 'edit'
    end
  end

  def destroy
    collection = Collection.find params[:id]
    collection.locked = true
    collection.save!
    flash[:success] = 'Collection successfully archived'
    redirect_to collections_path
  end

  def restore
    collection = Collection.find params[:id]
    collection.locked = false
    collection.save!
    flash[:success] = 'Collection successfully restored'
    redirect_to collections_path
  end

  def users
    @collection = Collection.includes(:users).find params[:id]
    @users = @collection.users if @collection
  end
end
