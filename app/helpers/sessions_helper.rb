module SessionsHelper
  def sign_in(user)
    cookies[:remember_token] = {
      value: user.remember_token,
      expires: 1.week.from_now.utc
    }
    self.current_user = user
  end

  def sign_out
    self.current_user = nil
    cookies.delete :remember_token
  end

  def signed_in?
    !current_user.nil?
  end

  def current_user=(user)
    @current_user = user
  end

  # If we've already identified the current user, return the user. Otherwise,
  # search for the use using the remember token in their cookie
  def current_user
    @current_user ||= User.includes(:collection).find_by(
      remember_token: cookies[:remember_token])
  end

  def current_user?(user)
    user == current_user
  end
end
