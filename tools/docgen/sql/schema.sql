-- phpMyAdmin SQL Dump
-- version 4.2.2
-- http://www.phpmyadmin.net
--
-- Host: db01
-- Generation Time: Jul 31, 2014 at 12:10 PM
-- Server version: 5.5.11-log
-- PHP Version: 5.5.13

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

--
-- Database: `smdocs`
--

-- --------------------------------------------------------

--
-- Table structure for table `sm_smconst`
--

CREATE TABLE IF NOT EXISTS `sm_smconst` (
`id` int(11) NOT NULL,
  `fileid` int(11) NOT NULL,
  `descrip` tinytext NOT NULL,
  `fulltext` text NOT NULL
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=141 ;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smdefine`
--

CREATE TABLE IF NOT EXISTS `sm_smdefine` (
  `id` int(11) NOT NULL,
  `variable` varchar(64) NOT NULL,
  `value` tinytext NOT NULL,
  `comment` tinytext NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfiles`
--

CREATE TABLE IF NOT EXISTS `sm_smfiles` (
  `id` int(11) NOT NULL,
  `name` varchar(32) NOT NULL,
  `filename` varchar(32) NOT NULL,
  `fcount` int(11) NOT NULL,
  `ccount` int(11) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfilescon`
--

CREATE TABLE IF NOT EXISTS `sm_smfilescon` (
  `id` int(11) NOT NULL,
  `cont` text NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smfunctions`
--

CREATE TABLE IF NOT EXISTS `sm_smfunctions` (
  `id` int(11) NOT NULL,
  `func` varchar(64) CHARACTER SET utf8 COLLATE utf8_bin NOT NULL,
  `fullfunc` text NOT NULL,
  `description` text NOT NULL,
  `treturn` tinytext NOT NULL,
  `onerror` tinytext NOT NULL,
  `funcinput` text NOT NULL,
  `exemple` text NOT NULL,
  `inc` int(11) NOT NULL,
  `incname` varchar(32) NOT NULL,
  `typeof` int(11) NOT NULL,
  `depreached` tinyint(1) NOT NULL,
  `version` varchar(32) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `sm_sminfo`
--

CREATE TABLE IF NOT EXISTS `sm_sminfo` (
  `master` varchar(32) NOT NULL,
  `infoa` int(11) NOT NULL,
  `infob` varchar(32) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `sm_smposts`
--

CREATE TABLE IF NOT EXISTS `sm_smposts` (
`id` int(11) NOT NULL,
  `file` int(11) NOT NULL,
  `func` int(11) NOT NULL,
  `time` int(11) NOT NULL,
  `poster` varchar(32) NOT NULL,
  `body` text NOT NULL,
  `ip` varchar(16) NOT NULL
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=311 ;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `sm_smconst`
--
ALTER TABLE `sm_smconst`
 ADD PRIMARY KEY (`id`), ADD KEY `fileid` (`fileid`);

--
-- Indexes for table `sm_smdefine`
--
ALTER TABLE `sm_smdefine`
 ADD PRIMARY KEY (`id`);

--
-- Indexes for table `sm_smfiles`
--
ALTER TABLE `sm_smfiles`
 ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `name` (`name`);

--
-- Indexes for table `sm_smfilescon`
--
ALTER TABLE `sm_smfilescon`
 ADD PRIMARY KEY (`id`);

--
-- Indexes for table `sm_smfunctions`
--
ALTER TABLE `sm_smfunctions`
 ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `name` (`func`), ADD KEY `inc` (`inc`);

--
-- Indexes for table `sm_sminfo`
--
ALTER TABLE `sm_sminfo`
 ADD KEY `master` (`master`);

--
-- Indexes for table `sm_smposts`
--
ALTER TABLE `sm_smposts`
 ADD PRIMARY KEY (`id`), ADD KEY `ip` (`ip`);

