import

gocode "time"
gocode "fmt"

def

Time = struct(year, month, day, hour, min, sec, nsec int, loc string)

Weekday = enum SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY

components(t Time) :
    t[year], t[month], t[day], t[hour], t[min], t[sec], t[nsec], t[loc]

add (t Time, duration int) :
    Time(goAdd(components(t), i))

goAdd(year, month, day, hour, min, sec, nsec int, loc string, d int) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).Add(time.Duration(d))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

addDate(t Time, years, months, days int) : 
    Time(goAddDate(components(t), years, months, days))

goAddDate(year, month, day, hour, min, sec, nsec int, loc string, years, months, days int) : gocode {
    newTime := goTime(year, month, day, hour, min, sec, nsec, loc).AddDate(years, months, days)
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

after(t, u Time) :
    goAfter(components(t), components(u))

goAfter(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, uyear, umonth, uday, uhour, umin, usec, unsec int, uloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).After(goTime(uyear, umonth, uday, uhour, umin, usec, unsec, uloc))
}

before(t, u Time) :
    goBefore(components(t), components(u))

goBefore(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, uyear, umonth, uday, uhour, umin, usec, unsec int, uloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Before(goTime(uyear, umonth, uday, uhour, umin, usec, unsec, uloc))
}

compare(t, u Time) :
    goCompare(components(t), components(u))

goCompare(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, uyear, umonth, uday, uhour, umin, usec, unsec int, uloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Compare(goTime(uyear, umonth, uday, uhour, umin, usec, unsec, uloc))
}

equal(t, u Time) :
    goEqual(components(t), components(u))

goEqual(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, uyear, umonth, uday, uhour, umin, usec, unsec int, uloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Equal(goTime(uyear, umonth, uday, uhour, umin, usec, unsec, uloc))
}

format(t Time, layout string) :
    goFormat(components(t), layout)

goFormat(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, layout string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Format(layout)
}

isDst(t Time) :
    goIsDST(components(t))

goIsDST(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).IsDST()
}

isoWeek(t Time) :
    goIsoWeek(components(t))

goIsoWeek(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).ISOWeek()
}

isZero(t Time) :
    goIsZero(components(t))

goIsZero(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).IsZero()
}

local(t Time) :
    Time(goLocal(components(t)))

goLocal(year, month, day, hour, min, sec, nsec int, loc string) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).Local()
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

parse(layout, value string) :
    Time(goParse(layout, value))

goParse(layout, value string) : gocode {
    newTime, err := time.Parse(layout, value)
    if err != nil {
        return err
    }
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

parseDuration(s string) : gocode {
    dur, err := time.ParseDuration(s)
    if err != nil {
        return err
    }
    return int(dur)
}

round(t Time, duration int) :
    Time(goRound(components(t), duration))

goRound(year, month, day, hour, min, sec, nsec int, loc string, d int) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).Round(time.Duration(d))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

sub(t, u Time) :
    goSub(components(t), components(u))

goSub(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string, uyear, umonth, uday, uhour, umin, usec, unsec int, uloc string) : gocode {
    return int(goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Sub(goTime(uyear, umonth, uday, uhour, umin, usec, unsec, uloc)))
}

timeIn(t Time, location string) :
    Time(goIn(components(t), location))

goIn(year, month, day, hour, min, sec, nsec int, loc string, location string) : gocode {
    newLoc, err := time.LoadLocation(location)
    if err != nil {
        return err
    }
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).In(newLoc)
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

timeToUnix(t Time) :
    goTimeToUnix(components(t))

goTimeToUnix(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).Unix()
}

timeToUnixMicro(t Time) :
    goTimeToUnixMicro(components(t))

goTimeToUnixMicro(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).UnixMicro()
}

timeToUnixMilli(t Time) :
    goTimeToUnixMilli(components(t))

goTimeToUnixMilli(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).UnixMilli()
}

timeToUnixNano(t Time) :
    goTimeToUnixNano(components(t))

goTimeToUnixNano(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).UnixNano()
}

truncate(t Time, duration int) :
    Time(goTruncate(components(t), duration))

goTruncate(year, month, day, hour, min, sec, nsec int, loc string, d int) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).Truncate(time.Duration(d))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

unixToTime(sec, usec int) :
    Time(goUnixToTime(sec, usec))

goUnixToTime(sec, usec int) : gocode {
    newTime := time.Unix(int64(sec), int64(usec))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

unixMicroToTime(usec int) :
    Time(goUnixMicroToTime(usec))

goUnixMicroToTime(usec int) : gocode {
    newTime := time.UnixMicro(int64(usec))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

unixMilliToTime(msec int) :
    Time(goUnixMilliToTime(msec))

goUnixMilliToTime(msec int) : gocode {
    newTime := time.UnixMilli(int64(msec))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

utc(t Time) :
    Time(goLocal(components(t)))

goUtc(year, month, day, hour, min, sec, nsec int, loc string) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).UTC()
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

weekday(t Time) :
    Weekday[goWeekday(components(t))]

goWeekday(year, month, day, hour, min, sec, nsec int, loc string) : gocode {
    return int(goTime(year, month, day, hour, min, sec, nsec, loc).Weekday())
}

yearDay(t Time) :
    goYearDay(components(t))

goYearDay(tyear, tmonth, tday, thour, tmin, tsec, tnsec int, tloc string) : gocode {
    return goTime(tyear, tmonth, tday, thour, tmin, tsec, tnsec, tloc).YearDay()
}

gocode {

    func charmMonth(m time.Month) int {
        return int(m)
    }

    func goTime(year, month, day, hour, min, sec, nsec int, loc string) time.Time {
        locObj, _ := time.LoadLocation(loc)
        var goMonth time.Month
        switch month {
        case 1 :
            goMonth = time.January
        case 2 :
            goMonth = time.February
        case 3 :
            goMonth = time.March
        case 4 :
            goMonth = time.April
        case 5 :
            goMonth = time.May
        case 6 :
            goMonth = time.June
        case 7 :
            goMonth = time.July
        case 8 :
            goMonth = time.August
        case 9 :
            goMonth = time.September
        case 10 :
            goMonth = time.October
        case 11 :
            goMonth = time.November
        case 12 :
            goMonth = time.December
        }
        return time.Date(year, goMonth, day, hour, min, sec, nsec, locObj)
    }

    func charmDurationToGoDuration(hours, mins, secs, nsecs int) time.Duration {
        dur, _ := time.ParseDuration(fmt.Sprintf("%vh%vm%vs%vns", hours, mins, secs, nsecs))
        return dur
    }
}


// func (t Time) Add(d Duration) Time
// func (t Time) AddDate(years int, months int, days int) Time
// func (t Time) After(u Time) bool
// func (t Time) AppendFormat(b []byte, layout string) []byte
// func (t Time) Before(u Time) bool
// func (t Time) Clock() (hour, min, sec int)
// func (t Time) Compare(u Time) int
// func (t Time) Date() (year int, month Month, day int)
// func (t Time) Day() int
// func (t Time) Equal(u Time) bool
// func (t Time) Format(layout string) string
// func (t Time) GoString() string
// func (t *Time) GobDecode(data []byte) error
// func (t Time) GobEncode() ([]byte, error)
// func (t Time) Hour() int
// func (t Time) ISOWeek() (year, week int)
// func (t Time) In(loc *Location) Time
// func (t Time) IsDST() bool
// func (t Time) IsZero() bool
// func (t Time) Local() Time
// func (t Time) Location() *Location
// func (t Time) MarshalBinary() ([]byte, error)
// func (t Time) MarshalJSON() ([]byte, error)
// func (t Time) MarshalText() ([]byte, error)
// func (t Time) Minute() int
// func (t Time) Month() Month
// func (t Time) Nanosecond() int
// func (t Time) Round(d Duration) Time
// func (t Time) Second() int
// func (t Time) String() string
// func (t Time) Sub(u Time) Duration
// func (t Time) Truncate(d Duration) Time
// func (t Time) UTC() Time
// func (t Time) Unix() int64
// func (t Time) UnixMicro() int64
// func (t Time) UnixMilli() int64
// func (t Time) UnixNano() int64
// func (t *Time) UnmarshalBinary(data []byte) error
// func (t *Time) UnmarshalJSON(data []byte) error
// func (t *Time) UnmarshalText(data []byte) error
// func (t Time) Weekday() Weekday
// func (t Time) Year() int
// func (t Time) YearDay() int
// func (t Time) Zone() (name string, offset int)
// func (t Time) ZoneBounds() (start, end Time)