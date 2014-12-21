module UsersHelper
  def follow_button(user)
    if current_user.following? user
      button_to 'Unfollow', user_follow_path(user), method: :delete
    else
      button_to 'Follow', user_follow_path(user)
    end
  end
end
