class UsersController < ApplicationController
  def index
    @users = User.all
  end

  def show
    @user = User.find params[:id]
  end

  def new
    @user = User.new
  end

  def create
    @user = User.new params[:user].permit %i(email first_name last_name
      password password_confirmation reporter uploader)
    if @user.save
      flash[:success] = 'New user successfully created'
      redirect_to @user
    else
      render :new
    end
  end
end
