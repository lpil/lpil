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
    @user = User.new params[:user].permit %i(email first_name last_name
      password password_confirmation reporter uploader admin)
    if @user.save
      flash[:success] = 'New user successfully created'
      redirect_to @user
    else
      render :new
    end
  end

  def update
    safe_fields = %i(email first_name last_name password password_confirmation)
    safe_fields + %i(reporter uploader admin) if current_user.admin?

    @user = User.find params[:id]
    if @user.update_attributes params[:user].permit safe_fields
      editing_self = @user == current_user
      flash[:success] = "#{@user.email} updated"
      sign_in @user if editing_self
      redirect_to @user
    else
      render 'edit'
    end
  end
end
