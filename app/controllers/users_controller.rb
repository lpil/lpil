class UsersController < ApplicationController
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
