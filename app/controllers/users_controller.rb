class UsersController < ApplicationController
  before_action :set_user, only: [:show, :edit, :update, :destroy]

  # Only signed in users can access the user resources
  before_filter :only_allow_signed_in_user

  # Allow admins to see and edit all user pages.
  # Allow regular users to see and edit their own user page
  before_filter only: [:show, :edit, :update] do
    block_access unless current_user.admin? ||
                        User.find(params[:id]) == current_user
  end

  # Only allow admins to create, destroy, and view all users
  before_filter only: [:new, :create, :index, :destroy] do
    block_access unless current_user.admin?
  end

  # GET /users
  def index
    @users = User.all
  end

  # GET /users/new
  def new
    @user = User.new
  end

  # GET /users/1
  def show
    @user = User.find params[:id]
  end

  # GET /users/1/edit
  def edit
    @user = User.find params[:id]
  end

  # POST /users
  def create
    @user = User.new params[:user].permit allowed_fields_for current_user
    if @user.save
      flash[:success] = 'New user successfully created'
      redirect_to @user
    else
      render :new
    end
  end

  # PATCH /users/1
  def update
    @user = User.find params[:id]
    if @user.update_attributes(
      params[:user].permit allowed_fields_for current_user)

      editing_self = @user == current_user
      flash[:success] = "#{@user.email} updated"
      sign_in @user if editing_self
      redirect_to @user
    else
      render 'edit'
    end
  end

  # DELETE /users/1
  def destroy
    user = User.find(params[:id])
    user.destroy
    flash[:success] = "User #{user.email} destroyed"
    redirect_to users_path
  end

  # GET /users/current
  def current
    redirect_to user_path current_user
  end

  private

  # Use callbacks to share common setup or constraints between actions.
  def set_user
    @user = User.find(params[:id])
  end

  def allowed_fields_for(current_user)
    return unless current_user
    fields = %i(email first_name last_name password password_confirmation
      collection_id)
    fields += %i(reporter uploader admin) if current_user.admin?
    fields
  end
end
