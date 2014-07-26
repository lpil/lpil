class UsersController < ApplicationController
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

  def index
    @users = User.all
  end

  def new
    @user = User.new
  end

  def show
    @user = User.find params[:id]
  end

  def edit
    @user = User.find params[:id]
  end

  def create
    @user = User.new params[:user].permit allowed_fields_for current_user
    if @user.save
      flash[:success] = 'New user successfully created'
      redirect_to @user
    else
      render :new
    end
  end

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

  def destroy
    user = User.find(params[:id])
    user.destroy
    flash[:success] = "User #{user.email} destroyed"
    redirect_to users_path
  end

  def current
    redirect_to user_path current_user
  end

  private

  def allowed_fields_for(current_user)
    return unless current_user
    fields = %i(email first_name last_name password password_confirmation)
    fields += %i(reporter uploader admin) if current_user.admin?
    fields
  end
end
