class CollectionsController < ApplicationController
  # Only admins can manipulate Collections
  before_filter do
    block_access unless current_user && current_user.admin?
  end

  def index
    @collections = Collection.all
  end

  def new
  end

  def show
  end

  def edit
  end

  def create
  end

  def destory
  end
end
