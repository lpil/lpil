class CollectionsController < ApplicationController
  # Only admins can manipulate Collections
  before_filter do
    block_access unless current_user && current_user.admin?
  end

  # GET /collections
  def index
    @collections = Collection.where locked: false
  end

  # GET /collections/archive
  def archive
    @collections = Collection.where locked: true
  end

  # GET /collections/new
  def new
    @collection = Collection.new
  end

  # GET /collections/1
  def show
    @collection = Collection.find params[:id]
  end

  # POST /collections
  def create
    @collection = Collection.new params[:collection].permit :name
    if @collection.save
      flash[:success] = 'New collection successfully created'
      redirect_to @collection
    else
      render :new, status: 400
    end
  end

  # GET /collections/1/edit
  def edit
    @collection = Collection.find params[:id]
  end

  # PATCH /collections/1
  def update
    @collection = Collection.find params[:id]
    if @collection.update_attributes(
      params[:collection].permit :name)

      flash[:success] = "#{@collection.name} successfully updated"
      redirect_to @collection
    else
      render 'edit', status: 400
    end
  end

  # DELETE /collections/1
  def destroy
    collection = Collection.find params[:id]
    collection.locked = true
    collection.save!
    flash[:success] = 'Collection successfully archived'
    redirect_to collections_path
  end

  # PATCH /collections/1/restore
  def restore
    collection = Collection.find params[:id]
    collection.locked = false
    collection.save!
    flash[:success] = 'Collection successfully restored'
    redirect_to collections_path
  end

  # GET /collections/1/users
  def users
    @collection = Collection.includes(:users).find params[:id]
    @users = @collection.users if @collection
  end
end
