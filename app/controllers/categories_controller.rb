class CategoriesController < ApplicationController
  # Only signed in users can access the user resources
  before_filter :only_allow_signed_in_user

  # Only allow admins to do most category stuff
  before_filter except: [:index, :show] do
    block_access unless current_user.admin?
  end

  # GET /categories
  def index
    @category = current_user.categories.first.root
    @tree = @category.subtree.arrange
    render :show
  end

  # GET /categories/new
  def new
    @category = Category.new
  end

  # GET /categories/1
  def show
    @category = Category.find_by id: params[:id]
    @tree = @category.root.subtree.arrange
    block_access unless current_user.admin? ||
      current_user.collection == @category.collection
  end

  # FIXME: list and view cause *a lot* of DB queries.
  # GET /categories/list
  def list
    @categories = current_user.categories.where locked: false
  end

  # GET /categories/archive
  def archive
    @categories = current_user.categories.where locked: true
  end

  # GET /categories/1/edit
  def edit
    @category = Category.find params[:id]
  end

  # POST /categories
  def create
    @category = Category.new params[:category].permit [:name, :parent_id]
    @category.collection = current_user.collection
    @category.locked = false
    if params[:category][:parent_id].nil?
      flash[:alert] = 'Please specify a parent'
      render :new, status: 400
    elsif @category.save
      flash[:success] = 'New category successfully created'
      redirect_to @category
    else
      render :new
    end
  end

  # PATCH /categories/1
  def update
    @category = Category.find params[:id]

    # Don't allow a non-root category to become a root category
    if !@category.root? && params[:category][:parent_id] == ''
      flash[:alert] = 'Non-root categories cannot become root!'
      render 'edit'
      return
    end

    if @category.update_attributes(
      params[:category].permit [:name, :parent_id]
    )
      flash[:success] = "#{@category.name} updated"
      redirect_to '/categories/list'
    else
      render 'edit'
    end
  end

  # DELETE /categories/1
  def destroy
    category = Category.find params[:id]
    category.locked = true
    category.save!
    flash[:success] = 'Category successfully archived'
    redirect_to '/categories/list'
  end

  # PATCH /categories/1/restore
  def restore
    category = Category.find params[:id]
    category.locked = false
    category.save!
    flash[:success] = 'Category successfully restored'
    redirect_to '/categories/list'
  end
end
