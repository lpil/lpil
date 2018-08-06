class Timeline
  # Make it an ActiveModel compatible object that returns a valid partial path
  extend ActiveModel::Naming

  def initialize(user)
    @user = user
  end

  def shouts
    Shout.where user_id: shout_user_ids
  end

  private

  def shout_user_ids
    [@user.id] + @user.followed_user_ids
  end
end
