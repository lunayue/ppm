package users

import tracker.Tracker

case class User(username: String, password: String, trackers:List[Tracker]){
  def addTracker(t:Tracker):User = User(this.username, this.password, t::this.trackers)
  def replaceTrackers(ts:List[Tracker]):User = User(this.username, this.password, ts)
  def replaceTracker(old:Tracker, novo:Tracker):User = User(this.username, this.password, this.trackers.updated(this.trackers.indexOf(old), novo))
}
