class CategoriesController < ApplicationController
  # Only signed in users can access the user resources
  before_filter :only_allow_signed_in_user

  # Only allow admins to do most category stuff
  before_filter except: [:index, :show] do
    block_access unless current_user.admin?
  end

  def index
    @category = current_user.categories.first.root
    @tree = @category.subtree.arrange
    render :show
  end

  def show
    @category = Category.find_by id: params[:id]
    @tree = @category.root.subtree.arrange
    block_access unless current_user.admin? ||
      current_user.collection == @category.collection
  end

  # FIXME: list and view cause *a lot* of DB queries.
  def list
    @categories = current_user.categories locked: false
  end

  def archive
    @collections = current_user.categories.where locked: true
  end

  def edit
    @category = Category.find params[:id]
  end

  def new
    @category = Category.new
  end

  def create
    unless params[:category][:parent_id].present?
      flash[:alert] = 'Please specify a parent'
      render :new
    end

    @category = Category.new params[:category].permit [:name, :parent_id]
    @category.collection = current_user.collection
    @category.locked = false
    if @category.save
      flash[:success] = 'New category successfully created'
      redirect_to @category
    else
      render :new
    end
  end

  def update
    @category = Category.find params[:id]

    # Don't allow a non-root category to become a root category
    if !@category.root? && params[:category][:parent_id] == ''
      require 'pry'; binding.pry
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

  def destroy
    category = Category.find params[:id]
    category.locked = true
    category.save!
    flash[:success] = 'Category successfully archived'
    redirect_to '/categories/list'
  end

  def restore
    category = Category.find params[:id]
    category.locked = false
    category.save!
    flash[:success] = 'Category successfully restored'
    redirect_to '/categories/list'
  end
end
